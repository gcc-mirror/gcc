/**
 * Read a file from disk and store it in memory.
 *
 * Copyright: Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/file_manager.d, _file_manager.d)
 * Documentation:  https://dlang.org/phobos/dmd_file_manager.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/file_manager.d
 */

module dmd.file_manager;

import dmd.root.stringtable : StringTable;
import dmd.root.file : File, Buffer;
import dmd.root.filename : FileName;
import dmd.root.string : toDString;
import dmd.globals;
import dmd.identifier;

enum package_d  = "package." ~ mars_ext;
enum package_di = "package." ~ hdr_ext;

/// Returns: whether a file with `name` is a special "package.d" module
bool isPackageFileName(scope FileName fileName) nothrow
{
    return FileName.equals(fileName.name, package_d) || FileName.equals(fileName.name, package_di);
}

final class FileManager
{
    private StringTable!(const(ubyte)[]) files;

    ///
    public this () nothrow
    {
        this.files._init();
    }

nothrow:
    /********************************************
    * Look for the source file if it's different from filename.
    * Look for .di, .d, directory, and along global.path.
    * Does not open the file.
    * Params:
    *      filename = as supplied by the user
    *      path = path to look for filename
    * Returns:
    *      the found file name or
    *      `null` if it is not different from filename.
    */
    static const(char)[] lookForSourceFile(const char[] filename, const char*[] path)
    {
        //printf("lookForSourceFile(`%.*s`)\n", cast(int)filename.length, filename.ptr);
        /* Search along path[] for .di file, then .d file, then .i file, then .c file.
        */
        const sdi = FileName.forceExt(filename, hdr_ext);
        if (FileName.exists(sdi) == 1)
            return sdi;
        scope(exit) FileName.free(sdi.ptr);

        const sd = FileName.forceExt(filename, mars_ext);
        // Special file name representing `stdin`, always assume its presence
        if (sd == "__stdin.d")
            return sd;
        if (FileName.exists(sd) == 1)
            return sd;
        scope(exit) FileName.free(sd.ptr);

        const si = FileName.forceExt(filename, i_ext);
        if (FileName.exists(si) == 1)
            return si;
        scope(exit) FileName.free(si.ptr);

        const sc = FileName.forceExt(filename, c_ext);
        if (FileName.exists(sc) == 1)
            return sc;
        scope(exit) FileName.free(sc.ptr);

        if (FileName.exists(filename) == 2)
        {
            /* The filename exists and it's a directory.
            * Therefore, the result should be: filename/package.d
            * iff filename/package.d is a file
            */
            const ni = FileName.combine(filename, package_di);
            if (FileName.exists(ni) == 1)
                return ni;
            FileName.free(ni.ptr);

            const n = FileName.combine(filename, package_d);
            if (FileName.exists(n) == 1)
                return n;
            FileName.free(n.ptr);
        }
        if (FileName.absolute(filename))
            return null;
        if (!path.length)
            return null;
        foreach (entry; path)
        {
            const p = entry.toDString();

            const(char)[] n = FileName.combine(p, sdi);
            if (FileName.exists(n) == 1) {
                return n;
            }
            FileName.free(n.ptr);

            n = FileName.combine(p, sd);
            if (FileName.exists(n) == 1) {
                return n;
            }
            FileName.free(n.ptr);

            n = FileName.combine(p, si);
            if (FileName.exists(n) == 1) {
                return n;
            }
            FileName.free(n.ptr);

            n = FileName.combine(p, sc);
            if (FileName.exists(n) == 1) {
                return n;
            }
            FileName.free(n.ptr);

            const b = FileName.removeExt(filename);
            n = FileName.combine(p, b);
            FileName.free(b.ptr);
            if (FileName.exists(n) == 2)
            {
                const n2i = FileName.combine(n, package_di);
                if (FileName.exists(n2i) == 1)
                    return n2i;
                FileName.free(n2i.ptr);
                const n2 = FileName.combine(n, package_d);
                if (FileName.exists(n2) == 1) {
                    return n2;
                }
                FileName.free(n2.ptr);
            }
            FileName.free(n.ptr);
        }
        return null;
    }

    /**
     * Looks up the given filename from the internal file buffer table.
     * If the file does not already exist within the table, it will be read from the filesystem.
     * If it has been read before,
     *
     * Returns: the loaded source file if it was found in memory,
     *      otherwise `null`
     */
    const(ubyte)[] lookup(FileName filename)
    {
        const name = filename.toString;
        if (auto val = files.lookup(name))
            return val.value;

        if (name == "__stdin.d")
        {
            auto buffer = readFromStdin().extractSlice();
            if (this.files.insert(name, buffer) is null)
                assert(0, "stdin: Insert after lookup failure should never return `null`");
            return buffer;
        }

        if (FileName.exists(name) != 1)
            return null;

        auto readResult = File.read(name);
        if (!readResult.success)
            return null;

        auto fb = readResult.extractSlice();
        if (files.insert(name, fb) is null)
            assert(0, "Insert after lookup failure should never return `null`");

        return fb;
    }

    /**
     * Looks up the given filename from the internal file buffer table, and returns the lines within the file.
     * If the file does not already exist within the table, it will be read from the filesystem.
     * If it has been read before,
     *
     * Returns: the loaded source file if it was found in memory,
     *      otherwise `null`
     */
    const(char)[][] getLines(FileName file)
    {
        const(char)[][] lines;
        if (const buffer = lookup(file))
        {
            const slice = buffer;
            size_t start, end;
            for (auto i = 0; i < slice.length; i++)
            {
                const c = slice[i];
                if (c == '\n' || c == '\r')
                {
                    if (i != 0)
                    {
                        end = i;
                        // Appending lines one at a time will certainly be slow
                        lines ~= cast(const(char)[])slice[start .. end];
                    }
                    // Check for Windows-style CRLF newlines
                    if (c == '\r')
                    {
                        if (slice.length > i + 1 && slice[i + 1] == '\n')
                        {
                            // This is a CRLF sequence, skip over two characters
                            start = i + 2;
                            i++;
                        }
                        else
                        {
                            // Just a CR sequence
                            start = i + 1;
                        }
                    }
                    else
                    {
                        // The next line should start after the LF sequence
                        start = i + 1;
                    }
                }
            }

            if (slice[$ - 1] != '\r' && slice[$ - 1] != '\n')
            {
                end = slice.length;
                lines ~= cast(const(char)[])slice[start .. end];
            }
        }

        return lines;
    }

    /**
     * Adds the contents of a file to the table.
     * Params:
     *  filename = name of the file
     *  buffer = contents of the file
     * Returns:
     *  the buffer added, or null
     */
    const(ubyte)[] add(FileName filename, const(ubyte)[] buffer)
    {
        auto val = files.insert(filename.toString, buffer);
        return val == null ? null : val.value;
    }
}

private Buffer readFromStdin() nothrow
{
    import core.stdc.stdio;
    import dmd.errors;
    import dmd.root.rmem;

    enum bufIncrement = 128 * 1024;
    size_t pos = 0;
    size_t sz = bufIncrement;

    ubyte* buffer = null;
    for (;;)
    {
        buffer = cast(ubyte*)mem.xrealloc(buffer, sz + 4); // +2 for sentinel and +2 for lexer

        // Fill up buffer
        do
        {
            assert(sz > pos);
            size_t rlen = fread(buffer + pos, 1, sz - pos, stdin);
            pos += rlen;
            if (ferror(stdin))
            {
                import core.stdc.errno;
                error(Loc.initial, "cannot read from stdin, errno = %d", errno);
                fatal();
            }
            if (feof(stdin))
            {
                // We're done
                assert(pos < sz + 2);
                buffer[pos .. pos + 4] = '\0';
                return Buffer(buffer[0 .. pos]);
            }
        } while (pos < sz);

        // Buffer full, expand
        sz += bufIncrement;
    }

    assert(0);
}

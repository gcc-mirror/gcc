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
import dmd.root.file : File, FileBuffer;
import dmd.root.filename : FileName;
import dmd.root.string : toDString;
import dmd.globals;
import dmd.identifier;

enum package_d  = "package." ~ mars_ext;
enum package_di = "package." ~ hdr_ext;

extern(C++) struct FileManager
{
    private StringTable!(FileBuffer*) files;
    private __gshared bool initialized = false;

nothrow:
    extern(D) private FileBuffer* readToFileBuffer(const(char)[] filename)
    {
        if (!initialized)
            FileManager._init();

        auto readResult = File.read(filename);
        if (readResult.success)
        {
            FileBuffer* fb;
            if (auto val = files.lookup(filename))
                fb = val.value;

            if (!fb)
                fb = FileBuffer.create();

            fb.data = readResult.extractSlice();

            return files.insert(filename, fb) == null ? null : fb;
        }
        else
        {
            return null;
        }

    }

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
    extern(D) static const(char)[] lookForSourceFile(const char[] filename, const char*[] path)
    {
        //printf("lookForSourceFile(`%.*s`)\n", cast(int)filename.length, filename.ptr);
        /* Search along path[] for .di file, then .d file, then .i file, then .c file.
        */
        const sdi = FileName.forceExt(filename, hdr_ext);
        if (FileName.exists(sdi) == 1)
            return sdi;
        scope(exit) FileName.free(sdi.ptr);

        const sd = FileName.forceExt(filename, mars_ext);
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
    extern(D) FileBuffer* lookup(FileName filename)
    {
        if (!initialized)
            FileManager._init();

        if (auto val = files.lookup(filename.toString))
        {
            // There is a chance that the buffer could've been
            // stolen by a reader with extractSlice, so we should
            // try and do our reading logic if that happens.
            if (val !is null && val.value.data !is null)
            {
                return val.value;
            }
        }

        const name = filename.toString;
        auto res = FileName.exists(name);
        if (res == 1)
            return readToFileBuffer(name);

        const fullName = lookForSourceFile(name, global.path ? (*global.path)[] : null);
        if (!fullName)
            return null;

        return readToFileBuffer(fullName);
    }

    extern(C++) FileBuffer* lookup(const(char)* filename)
    {
        return lookup(FileName(filename.toDString));
    }

    /**
     * Looks up the given filename from the internal file buffer table, and returns the lines within the file.
     * If the file does not already exist within the table, it will be read from the filesystem.
     * If it has been read before,
     *
     * Returns: the loaded source file if it was found in memory,
     *      otherwise `null`
     */
    extern(D) const(char)[][] getLines(FileName file)
    {
        if (!initialized)
            FileManager._init();

        const(char)[][] lines;
        if (FileBuffer* buffer = lookup(file))
        {
            ubyte[] slice = buffer.data[0 .. buffer.data.length];
            size_t start, end;
            ubyte c;
            for (auto i = 0; i < slice.length; i++)
            {
                c = slice[i];
                if (c == '\n' || c == '\r')
                {
                    if (i != 0)
                    {
                        end = i;
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
     * Adds a FileBuffer to the table.
     *
     * Returns: The FileBuffer added, or null
     */
    extern(D) FileBuffer* add(FileName filename, FileBuffer* filebuffer)
    {
        if (!initialized)
            FileManager._init();

        auto val = files.insert(filename.toString, filebuffer);
        return val == null ? null : val.value;
    }

    extern(C++) FileBuffer* add(const(char)* filename, FileBuffer* filebuffer)
    {
        if (!initialized)
            FileManager._init();

        auto val = files.insert(filename.toDString, filebuffer);
        return val == null ? null : val.value;
    }

    __gshared fileManager = FileManager();

    // Initialize the global FileManager singleton
    extern(C++) static __gshared void _init()
    {
        if (!initialized)
        {
            fileManager.initialize();
            initialized = true;
        }
    }

    void initialize()
    {
        files._init();
    }
}

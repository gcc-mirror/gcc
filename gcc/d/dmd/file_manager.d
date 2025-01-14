/**
 * Read a file from disk and store it in memory.
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/file_manager.d, _file_manager.d)
 * Documentation:  https://dlang.org/phobos/dmd_file_manager.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/file_manager.d
 */

module dmd.file_manager;

import core.stdc.stdio;
import dmd.common.outbuffer;
import dmd.root.stringtable : StringTable;
import dmd.root.file : File, Buffer;
import dmd.root.filename : FileName, isDirSeparator;
import dmd.root.string : toDString;
import dmd.errors;
import dmd.globals;
import dmd.identifier;
import dmd.location;

enum package_d  = "package." ~ mars_ext;
enum package_di = "package." ~ hdr_ext;

/// Returns: whether a file with `name` is a special "package.d" module
bool isPackageFileName(scope FileName fileName) nothrow
{
    return FileName.equals(fileName.name, package_d) || FileName.equals(fileName.name, package_di);
}

// A path stack that allows one to go up and down the path using directory
// separators. `cur` is the current path, `up` goes up one path, `down` goes
// down one path. if `up` or `down` return false, there are no further paths.
private struct PathStack
{
    private const(char)[] path;
    private size_t pos;

    @safe @nogc nothrow pure:

    this(const(char)[] p)
    {
        path = p;
        pos = p.length;
    }

    const(char)[] cur()
    {
        return path[0 .. pos];
    }

    bool up()
    {
        if (pos == 0)
            return false;
        while (--pos != 0)
            if (isDirSeparator(path[pos]))
                return true;
        return false;
    }

    bool down()
    {
        if (pos == path.length)
            return false;
        while (++pos != path.length)
            if (isDirSeparator(path[pos]))
                return true;
        return false;
    }
}

/***************************
 * Cache path lookups so the operating system
 * is only consulted once for each path.
 */
private struct PathCache
{
    /* for filespec "a/b/c/d.ext"
     * a b and c are directories, a, a/b, a/b/c are paths.
     */

    StringTable!(bool) pathStatus;      // cached value of does a path exist or not

  nothrow:

    /**
     * Determine if the path part of path/filename exists.
     * Cache the results for the path and each left-justified subpath of the path.
     * Params:
     *  filespec = path/filename
     * Returns:
     *  true if path exists, false if it does not
     */
    bool pathExists(const(char)[] filespec) nothrow
    {
        /* look for the longest leftmost parent path that is cached
         * by starting at the right and working to the left
         */
        bool exists = true;
        auto st = PathStack(filespec);
        while (st.up) {
            if (auto cached = pathStatus.lookup(st.cur)) {
                exists = cached.value;
                break;
            }
        }
        /* found a parent path that is cached (or reached the left end of the path).
         * Now move right caching the results of those directories.
         * Once a directory is found to not exist, all the directories
         * to the right of it do not exist
         */
        while (st.down) {
            if (!exists)
                pathStatus.insert(st.cur, false);
            else
                exists = pathStatus.insert(st.cur, FileName.exists(st.cur) == 2).value;
        }

        return exists;
    }

    /**
     * Ask if path ends in a directory.
     * Cache result for speed.
     * Params:
     *  path = a path
     * Returns:
     *  true if it's a path, false if not
     */
    bool isExistingPath(const char[] path)
    {
        auto cached = pathStatus.lookup(path);
        if (!cached)
            cached = pathStatus.insert(path, FileName.exists(path) == 2);
        return cached.value;
    }
}

final class FileManager
{
    private StringTable!(const(ubyte)[]) files;  // contents of files indexed by file name

    private PathCache pathCache;

    ///
    public this () nothrow
    {
        this.files._init();
        this.pathCache.pathStatus._init();
    }

nothrow:
    /********************************************
    * Look for the source file if it's different from filename.
    * Look for .di, .d, directory, and along global.path.
    * Does not open the file.
    * Params:
    *      filename = as supplied by the user
    *      pathsInfo = pathsInfo to look for filename with metadata
    *      whichPathFoundThis = Which path from `path` was used in determining the output path, or -1 if unknown.
    * Returns:
    *      the found file name or
    *      `null` if it is not different from filename.
    */
    const(char)[] lookForSourceFile(const char[] filename, const ImportPathInfo[] pathsInfo, out ptrdiff_t whichPathFoundThis)
    {
        //printf("lookForSourceFile(`%.*s`)\n", cast(int)filename.length, filename.ptr);
        /* Search along pathsInfo[] for .di file, then .d file.
        */

        whichPathFoundThis = -1;

        // see if we should check for the module locally.
        bool checkLocal = pathCache.pathExists(filename);
        const sdi = FileName.forceExt(filename, hdr_ext);
        if (checkLocal && FileName.exists(sdi) == 1)
            return sdi;
        scope(exit) FileName.free(sdi.ptr);

        const sd = FileName.forceExt(filename, mars_ext);
        // Special file name representing `stdin`, always assume its presence
        if (sd == "__stdin.d")
            return sd;
        if (checkLocal && FileName.exists(sd) == 1)
            return sd;
        scope(exit) FileName.free(sd.ptr);

        if (checkLocal)
        {
            if (pathCache.isExistingPath(filename))
            {
                /* The filename exists but it's a directory.
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
        }

        if (FileName.absolute(filename))
            return null;
        if (!pathsInfo.length)
            return null;
        foreach (pathIndex, entry; pathsInfo)
        {
            const p = entry.path.toDString();

            const(char)[] n = FileName.combine(p, sdi);

            if (!pathCache.pathExists(n)) {
                FileName.free(n.ptr);
                continue; // no need to check for anything else.
            }
            if (FileName.exists(n) == 1) {
                return n;
            }
            FileName.free(n.ptr);

            n = FileName.combine(p, sd);
            if (FileName.exists(n) == 1) {
                whichPathFoundThis = pathIndex;
                return n;
            }
            FileName.free(n.ptr);

            n = FileName.combine(p, FileName.sansExt(filename));
            scope(exit) FileName.free(n.ptr);

            // also cache this if we are looking for package.d[i]
            if (pathCache.isExistingPath(n))
            {
                const n2i = FileName.combine(n, package_di);
                if (FileName.exists(n2i) == 1) {
                    whichPathFoundThis = pathIndex;
                    return n2i;
                }

                FileName.free(n2i.ptr);
                const n2 = FileName.combine(n, package_d);
                if (FileName.exists(n2) == 1) {
                    whichPathFoundThis = pathIndex;
                    return n2;
                }
                FileName.free(n2.ptr);
            }
        }

        /* ImportC: No D modules found, now search along paths[] for .i file, then .c file.
         */
        const si = FileName.forceExt(filename, i_ext);
        if (FileName.exists(si) == 1)
            return si;
        scope(exit) FileName.free(si.ptr);

        const sc = FileName.forceExt(filename, c_ext);
        if (FileName.exists(sc) == 1)
            return sc;
        scope(exit) FileName.free(sc.ptr);
        foreach (pathIndex, entry; pathsInfo)
        {
            const p = entry.path.toDString();

            const(char)[] n = FileName.combine(p, si);
            if (FileName.exists(n) == 1) {
                whichPathFoundThis = pathIndex;
                return n;
            }
            FileName.free(n.ptr);

            n = FileName.combine(p, sc);
            if (FileName.exists(n) == 1) {
                whichPathFoundThis = pathIndex;
                return n;
            }
            FileName.free(n.ptr);
        }
        return null;
    }

    /**
     * Retrieve the cached contents of the file given by `filename`.
     * If the file has not been read before, read it and add the contents
     * to the file cache.
     * Params:
     *  filename = the name of the file
     * Returns:
     *  the contents of the file, or `null` if it could not be read or was empty
     */
    const(ubyte)[] getFileContents(FileName filename)
    {
        const name = filename.toString;
        if (auto val = files.lookup(name))      // if `name` is cached
            return val.value;                   // return its contents

        OutBuffer buf;
        if (name == "__stdin.d")                // special name for reading from stdin
        {
            if (readFromStdin(buf))
                fatal();
        }
        else
        {
            if (FileName.exists(name) != 1) // if not an ordinary file
                return null;

            if (File.read(name, buf))
                return null;        // failed
        }

        buf.write32(0);         // terminating dchar 0

        const length = buf.length;
        const ubyte[] fb = cast(ubyte[])(buf.extractSlice()[0 .. length - 4]);
        if (files.insert(name, fb) is null)
            assert(0, "Insert after lookup failure should never return `null`");

        return fb;
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

private bool readFromStdin(ref OutBuffer sink) nothrow
{
    import core.stdc.stdio;
    import dmd.errors;

    enum BufIncrement = 128 * 1024;

    for (size_t j; 1; ++j)
    {
        char[] buffer = sink.allocate(BufIncrement);

        // Fill up buffer
        size_t filled = 0;
        do
        {
            filled += fread(buffer.ptr + filled, 1, buffer.length - filled, stdin);
            if (ferror(stdin))
            {
                import core.stdc.errno;
                error(Loc.initial, "cannot read from stdin, errno = %d", errno);
                return true;
            }
            if (feof(stdin)) // successful completion
            {
                sink.setsize(j * BufIncrement + filled);
                return false;
            }
        } while (filled < BufIncrement);
    }

    assert(0);
}

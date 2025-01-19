/**
 * Encapsulates file/line/column locations.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/location.d, _location.d)
 * Documentation:  https://dlang.org/phobos/dmd_location.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/location.d
 */

module dmd.location;

import core.stdc.stdio;

import dmd.common.outbuffer;
import dmd.root.array;
import dmd.root.filename;
import dmd.root.string: toDString;

version (DMDLIB)
{
    version = LocOffset;
}

/// How code locations are formatted for diagnostic reporting
enum MessageStyle : ubyte
{
    digitalmars,  /// filename.d(line): message
    gnu,          /// filename.d:line: message, see https://www.gnu.org/prep/standards/html_node/Errors.html
    sarif         /// JSON SARIF output, see https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html
}
/**
A source code location

Used for error messages, `__FILE__` and `__LINE__` tokens, `__traits(getLocation, XXX)`,
debug info etc.
*/
struct Loc
{
    private uint _linnum;
    private uint _charnum;
    private uint fileIndex; // index into filenames[], starting from 1 (0 means no filename)
    version (LocOffset)
        uint fileOffset; /// utf8 code unit index relative to start of file, starting from 0

    static immutable Loc initial; /// use for default initialization of const ref Loc's

    extern (C++) __gshared bool showColumns;
    extern (C++) __gshared MessageStyle messageStyle;

    __gshared Array!(const(char)*) filenames;

nothrow:

    /*******************************
     * Configure how display is done
     * Params:
     *  showColumns = when to display columns
     *  messageStyle = digitalmars or gnu style messages
     */
    extern (C++) static void set(bool showColumns, MessageStyle messageStyle)
    {
        this.showColumns = showColumns;
        this.messageStyle = messageStyle;
    }

    extern (C++) this(const(char)* filename, uint linnum, uint charnum) @safe
    {
        this._linnum = linnum;
        this._charnum = charnum;
        this.filename = filename;
    }

    /// utf8 code unit index relative to start of line, starting from 1
    extern (C++) uint charnum() const @nogc @safe
    {
        return _charnum;
    }

    /// ditto
    extern (C++) uint charnum(uint num) @nogc @safe
    {
        return _charnum = num;
    }

    /// line number, starting from 1
    extern (C++) uint linnum() const @nogc @safe
    {
        return _linnum;
    }

    /// ditto
    extern (C++) uint linnum(uint num) @nogc @safe
    {
        return _linnum = num;
    }

    /***
     * Returns: filename for this location, null if none
     */
    extern (C++) const(char)* filename() const @nogc
    {
        return fileIndex ? filenames[fileIndex - 1] : null;
    }

    /***
     * Set file name for this location
     * Params:
     *   name = file name for location, null for no file name
     */
    extern (C++) void filename(const(char)* name) @trusted
    {
        if (name)
        {
            //printf("setting %s\n", name);
            filenames.push(name);
            fileIndex = cast(uint)filenames.length;
            assert(fileIndex, "internal compiler error: file name index overflow");
        }
        else
            fileIndex = 0;
    }

    extern (C++) const(char)* toChars(
        bool showColumns = Loc.showColumns,
        MessageStyle messageStyle = Loc.messageStyle) const nothrow
    {
        OutBuffer buf;
        writeSourceLoc(buf, SourceLoc(this), showColumns, messageStyle);
        return buf.extractChars();
    }

    /**
     * Checks for equivalence by comparing the filename contents (not the pointer) and character location.
     *
     * Note:
     *  - Uses case-insensitive comparison on Windows
     *  - Ignores `charnum` if `Columns` is false.
     */
    extern (C++) bool equals(ref const(Loc) loc) const
    {
        return (!showColumns || charnum == loc.charnum) &&
               linnum == loc.linnum &&
               FileName.equals(filename, loc.filename);
    }

    /**
     * `opEquals()` / `toHash()` for AA key usage
     *
     * Compare filename contents (case-sensitively on Windows too), not
     * the pointer - a static foreach loop repeatedly mixing in a mixin
     * may lead to multiple equivalent filenames (`foo.d-mixin-<line>`),
     * e.g., for test/runnable/test18880.d.
     */
    extern (D) bool opEquals(ref const(Loc) loc) const @trusted nothrow @nogc
    {
        import core.stdc.string : strcmp;

        return charnum == loc.charnum &&
               linnum == loc.linnum &&
               (filename == loc.filename ||
                (filename && loc.filename && strcmp(filename, loc.filename) == 0));
    }

    /// ditto
    extern (D) size_t toHash() const @trusted nothrow
    {
        import dmd.root.string : toDString;

        auto hash = hashOf(linnum);
        hash = hashOf(charnum, hash);
        hash = hashOf(filename.toDString, hash);
        return hash;
    }

    /******************
     * Returns:
     *   true if Loc has been set to other than the default initialization
     */
    bool isValid() const pure @safe
    {
        return fileIndex != 0;
    }
}

/**
 * Format a source location for error messages
 *
 * Params:
 *   buf = buffer to write string into
 *   loc = source location to write
 *   showColumns = include column number in message
 *   messageStyle = select error message format
 */
void writeSourceLoc(ref OutBuffer buf,
    SourceLoc loc,
    bool showColumns,
    MessageStyle messageStyle) nothrow
{
    buf.writestring(loc.filename);
    if (loc.line == 0)
        return;

    final switch (messageStyle)
    {
        case MessageStyle.digitalmars:
            buf.writeByte('(');
            buf.print(loc.line);
            if (showColumns && loc.column)
            {
                buf.writeByte(',');
                buf.print(loc.column);
            }
            buf.writeByte(')');
            break;
        case MessageStyle.gnu: // https://www.gnu.org/prep/standards/html_node/Errors.html
            buf.writeByte(':');
            buf.print(loc.line);
            if (showColumns && loc.column)
            {
                buf.writeByte(':');
                buf.print(loc.column);
            }
            break;
        case MessageStyle.sarif: // https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html
            // No formatting needed here for SARIF
            break;
    }
}

/**
 * Describes a location in the source code as a file + line number + column number
 *
 * While `Loc` is a compact opaque location meant to be stored in the AST,
 * this struct has simple modifiable fields and is used for printing.
 */
struct SourceLoc
{
    const(char)[] filename; /// name of source file
    uint line; /// line number (starts at 1)
    uint column; /// column number (starts at 1)

    // aliases for backwards compatibility
    alias linnum = line;
    alias charnum = column;

    this(const(char)[] filename, uint line, uint column) nothrow
    {
        this.filename = filename;
        this.line = line;
        this.column = column;
    }

    this(Loc loc) nothrow
    {
        this.filename = loc.filename.toDString();
        this.line = loc.linnum;
        this.column = loc.charnum;
    }
}

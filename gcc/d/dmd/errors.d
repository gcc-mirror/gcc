/**
 * Functions for raising errors.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/errors.d, _errors.d)
 * Documentation:  https://dlang.org/phobos/dmd_errors.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/errors.d
 */

module dmd.errors;

public import core.stdc.stdarg;
public import dmd.root.string: fTuple;
import dmd.errorsink;
import dmd.globals;
import dmd.location;
import dmd.root.string;

nothrow:

/// Constants used to discriminate kinds of error messages.
enum ErrorKind
{
    warning,
    deprecation,
    error,
    tip,
    message,
}

/***************************
 * Error message sink for D compiler.
 */
class ErrorSinkCompiler : ErrorSink
{
  nothrow:
  extern (C++):
  override:

    void verror(Loc loc, const(char)* format, va_list ap)
    {
        verrorReport(loc, format, ap, ErrorKind.error);
    }

    void verrorSupplemental(Loc loc, const(char)* format, va_list ap)
    {
        verrorReportSupplemental(loc, format, ap, ErrorKind.error);
    }

    void vwarning(Loc loc, const(char)* format, va_list ap)
    {
        verrorReport(loc, format, ap, ErrorKind.warning);
    }

    void vwarningSupplemental(Loc loc, const(char)* format, va_list ap)
    {
        verrorReportSupplemental(loc, format, ap, ErrorKind.warning);
    }

    void vdeprecation(Loc loc, const(char)* format, va_list ap)
    {
        verrorReport(loc, format, ap, ErrorKind.deprecation);
    }

    void vdeprecationSupplemental(Loc loc, const(char)* format, va_list ap)
    {
        verrorReportSupplemental(loc, format, ap, ErrorKind.deprecation);
    }

    void vmessage(Loc loc, const(char)* format, va_list ap)
    {
        verrorReport(loc, format, ap, ErrorKind.message);
    }
}


/**
 * Color highlighting to classify messages
 */
enum Classification : Color
{
    error = Color.brightRed,          /// for errors
    gagged = Color.brightBlue,        /// for gagged errors
    warning = Color.brightYellow,     /// for warnings
    deprecation = Color.brightCyan,   /// for deprecations
    tip = Color.brightGreen,          /// for tip messages
}

enum Color : int
{
    black         = 0,
    red           = 1,
    green         = 2,
    blue          = 4,
    yellow        = red | green,
    magenta       = red | blue,
    cyan          = green | blue,
    lightGray     = red | green | blue,
    bright        = 8,
    darkGray      = bright | black,
    brightRed     = bright | red,
    brightGreen   = bright | green,
    brightBlue    = bright | blue,
    brightYellow  = bright | yellow,
    brightMagenta = bright | magenta,
    brightCyan    = bright | cyan,
    white         = bright | lightGray,
}


static if (__VERSION__ < 2092)
    private extern (C++) void noop(Loc loc, const(char)* format, ...) {}
else
    pragma(printf) private extern (C++) void noop(Loc loc, const(char)* format, ...) {}


package auto previewErrorFunc(bool isDeprecated, FeatureState featureState) @safe @nogc pure nothrow
{
    with (FeatureState) final switch (featureState)
    {
        case enabled:
            return &error;

        case disabled:
            return &noop;

        case default_:
            return isDeprecated ? &noop : &deprecation;
    }
}

package auto previewSupplementalFunc(bool isDeprecated, FeatureState featureState) @safe @nogc pure nothrow
{
    with (FeatureState) final switch (featureState)
    {
        case enabled:
            return &errorSupplemental;

        case disabled:
            return &noop;

        case default_:
            return isDeprecated ? &noop : &deprecationSupplemental;
    }
}


/**
 * Print an error message, increasing the global error count.
 * Params:
 *      loc    = location of error
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void error(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.error);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void error(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.error);
        va_end(ap);
    }

/**
 * Same as above, but takes a filename and line information arguments as separate parameters.
 * Params:
 *      filename = source file of error
 *      linnum   = line in the source file
 *      charnum  = column number on the line
 *      format   = printf-style format specification
 *      ...      = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void error(const(char)* filename, uint linnum, uint charnum, const(char)* format, ...)
    {
        const loc = SourceLoc(filename.toDString, linnum, charnum);
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.error);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void error(const(char)* filename, uint linnum, uint charnum, const(char)* format, ...)
    {
        const loc = SourceLoc(filename.toDString, linnum, charnum);
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.error);
        va_end(ap);
    }

/// Callback for when the backend wants to report an error
extern(C++) void errorBackend(const(char)* filename, uint linnum, uint charnum, const(char)* format, ...)
{
    const loc = SourceLoc(filename.toDString, linnum, charnum);
    va_list ap;
    va_start(ap, format);
    verrorReport(loc, format, ap, ErrorKind.error);
    va_end(ap);
}

/**
 * Print additional details about an error message.
 * Doesn't increase the error count or print an additional error prefix.
 * Params:
 *      loc    = location of error
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void errorSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReportSupplemental(loc, format, ap, ErrorKind.error);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void errorSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReportSupplemental(loc, format, ap, ErrorKind.error);
        va_end(ap);
    }

/**
 * Print a warning message, increasing the global warning count.
 * Params:
 *      loc    = location of warning
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void warning(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.warning);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void warning(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.warning);
        va_end(ap);
    }

/**
 * Print additional details about a warning message.
 * Doesn't increase the warning count or print an additional warning prefix.
 * Params:
 *      loc    = location of warning
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void warningSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReportSupplemental(loc, format, ap, ErrorKind.warning);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void warningSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReportSupplemental(loc, format, ap, ErrorKind.warning);
        va_end(ap);
    }

/**
 * Print a deprecation message, may increase the global warning or error count
 * depending on whether deprecations are ignored.
 * Params:
 *      loc    = location of deprecation
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void deprecation(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.deprecation);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void deprecation(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.deprecation);
        va_end(ap);
    }

/**
 * Print additional details about a deprecation message.
 * Doesn't increase the error count, or print an additional deprecation prefix.
 * Params:
 *      loc    = location of deprecation
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void deprecationSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReportSupplemental(loc, format, ap, ErrorKind.deprecation);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void deprecationSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReportSupplemental(loc, format, ap, ErrorKind.deprecation);
        va_end(ap);
    }

/**
 * Print a verbose message.
 * Doesn't prefix or highlight messages.
 * Params:
 *      loc    = location of message
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void message(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.message);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void message(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(loc, format, ap, ErrorKind.message);
        va_end(ap);
    }

/**
 * Same as above, but doesn't take a location argument.
 * Params:
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void message(const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(Loc.initial, format, ap, ErrorKind.message);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void message(const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(Loc.initial, format, ap, ErrorKind.message);
        va_end(ap);
    }

/**
 * The type of the diagnostic handler
 * see verrorReport for arguments
 * Returns: true if error handling is done, false to continue printing to stderr
 */
alias DiagnosticHandler = bool delegate(const ref SourceLoc location, Color headerColor, const(char)* header, const(char)* messageFormat, va_list args, const(char)* prefix1, const(char)* prefix2);

/**
 * The diagnostic handler.
 * If non-null it will be called for every diagnostic message issued by the compiler.
 * If it returns false, the message will be printed to stderr as usual.
 */
__gshared DiagnosticHandler diagnosticHandler;

/**
 * Print a tip message with the prefix and highlighting.
 * Params:
 *      format = printf-style format specification
 *      ...    = printf-style variadic arguments
 */
static if (__VERSION__ < 2092)
    extern (C++) void tip(const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(Loc.initial, format, ap, ErrorKind.tip);
        va_end(ap);
    }
else
    pragma(printf) extern (C++) void tip(const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorReport(Loc.initial, format, ap, ErrorKind.tip);
        va_end(ap);
    }


/**
 * Implements $(D error), $(D warning), $(D deprecation), $(D message), and
 * $(D tip). Report a diagnostic error, taking a va_list parameter, and
 * optionally additional message prefixes. Whether the message gets printed
 * depends on runtime values of DiagnosticReporting and global gagging.
 * Params:
 *      loc         = location of error
 *      format      = printf-style format specification
 *      ap          = printf-style variadic arguments
 *      kind        = kind of error being printed
 *      p1          = additional message prefix
 *      p2          = additional message prefix
 */
private extern(C++) void verrorReport(Loc loc, const(char)* format, va_list ap, ErrorKind kind, const(char)* p1 = null, const(char)* p2 = null)
{
    return verrorReport(loc.SourceLoc, format, ap, kind, p1, p2);
}

/// ditto
private extern(C++) void verrorReport(const SourceLoc loc, const(char)* format, va_list ap, ErrorKind kind, const(char)* p1 = null, const(char)* p2 = null);

/**
 * Implements $(D errorSupplemental), $(D warningSupplemental), and
 * $(D deprecationSupplemental). Report an addition diagnostic error, taking a
 * va_list parameter. Whether the message gets printed depends on runtime
 * values of DiagnosticReporting and global gagging.
 * Params:
 *      loc         = location of error
 *      format      = printf-style format specification
 *      ap          = printf-style variadic arguments
 *      kind        = kind of error being printed
 */
private extern(C++) void verrorReportSupplemental(Loc loc, const(char)* format, va_list ap, ErrorKind kind)
{
    return verrorReportSupplemental(loc.SourceLoc, format, ap, kind);
}

/// ditto
private extern(C++) void verrorReportSupplemental(const SourceLoc loc, const(char)* format, va_list ap, ErrorKind kind);

/**
 * The type of the fatal error handler
 * Returns: true if error handling is done, false to do exit(EXIT_FAILURE)
 */
alias FatalErrorHandler = bool delegate();

/**
 * The fatal error handler.
 * If non-null it will be called for every fatal() call issued by the compiler.
 */
__gshared FatalErrorHandler fatalErrorHandler;

/**
 * Call this after printing out fatal error messages to clean up and exit the
 * compiler. You can also set a fatalErrorHandler to override this behaviour.
 */
extern (C++) void fatal();

/**
 * Try to stop forgetting to remove the breakpoints from
 * release builds.
 */
extern (C++) void halt() @safe;

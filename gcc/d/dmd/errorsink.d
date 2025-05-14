/**
 * Provides an abstraction for what to do with error messages.
 *
 * Copyright:   Copyright (C) 2023-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/errorsink.d, _errorsink.d)
 * Documentation:  https://dlang.org/phobos/dmd_errorsink.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/errorsink.d
 */

module dmd.errorsink;

import core.stdc.stdarg;

import dmd.location;

/***************************************
 * Where error/warning/deprecation messages go.
 */
abstract class ErrorSink
{
  nothrow:
  extern (C++):

    void verror(Loc loc, const(char)* format, va_list ap);
    void verrorSupplemental(Loc loc, const(char)* format, va_list ap);
    void vwarning(Loc loc, const(char)* format, va_list ap);
    void vwarningSupplemental(Loc loc, const(char)* format, va_list ap);
    void vmessage(Loc loc, const(char)* format, va_list ap);
    void vdeprecation(Loc loc, const(char)* format, va_list ap);
    void vdeprecationSupplemental(Loc loc, const(char)* format, va_list ap);

    void error(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verror(loc, format, ap);
        va_end(ap);
    }

    void errorSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        verrorSupplemental(loc, format, ap);
        va_end(ap);
    }

    void warning(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        vwarning(loc, format, ap);
        va_end(ap);
    }

    void warningSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        vwarningSupplemental(loc, format, ap);
        va_end(ap);
    }

    void message(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        vmessage(loc, format, ap);
        va_end(ap);
    }

    void deprecation(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        vdeprecation(loc, format, ap);
        va_end(ap);
    }

    void deprecationSupplemental(Loc loc, const(char)* format, ...)
    {
        va_list ap;
        va_start(ap, format);
        vdeprecationSupplemental(loc, format, ap);
        va_end(ap);
    }

    /**
     * This will be called to indicate compilation has either
     * finished or terminated, no more errors are possible - it's
     * now the time to print any stored errors.
     *
     * The default implementation does nothing since most error sinks have no state
     */
    void plugSink() {}
}

/*****************************************
 * Just ignores the messages.
 */
class ErrorSinkNull : ErrorSink
{
  nothrow:
  extern (C++):
  override:

    void verror(Loc loc, const(char)* format, va_list ap) { }

    void verrorSupplemental(Loc loc, const(char)* format, va_list ap) { }

    void vwarning(Loc loc, const(char)* format, va_list ap) { }

    void vwarningSupplemental(Loc loc, const(char)* format, va_list ap) { }

    void vmessage(Loc loc, const(char)* format, va_list ap) { }

    void vdeprecation(Loc loc, const(char)* format, va_list ap) { }

    void vdeprecationSupplemental(Loc loc, const(char)* format, va_list ap) { }
}

/*****************************************
 * Ignores the messages, but sets `sawErrors` for any calls to `error()`
 */
class ErrorSinkLatch : ErrorSinkNull
{
  nothrow:
  extern (C++):
  override:

    bool sawErrors;

    void verror(Loc loc, const(char)* format, va_list ap) { sawErrors = true; }
}

/*****************************************
 * Simplest implementation, just sends messages to stderr.
 * See also: ErrorSinkCompiler.
 */
class ErrorSinkStderr : ErrorSink
{
    import core.stdc.stdio;
    import core.stdc.stdarg;

  nothrow:
  extern (C++):
  override:

    void verror(Loc loc, const(char)* format, va_list ap)
    {
        fputs("Error: ", stderr);
        const p = loc.toChars();
        if (*p)
        {
            fprintf(stderr, "%s: ", p);
            //mem.xfree(cast(void*)p); // loc should provide the free()
        }

        vfprintf(stderr, format, ap);
        fputc('\n', stderr);
    }

    void verrorSupplemental(Loc loc, const(char)* format, va_list ap) { }

    void vwarning(Loc loc, const(char)* format, va_list ap)
    {
        fputs("Warning: ", stderr);
        const p = loc.toChars();
        if (*p)
        {
            fprintf(stderr, "%s: ", p);
            //mem.xfree(cast(void*)p); // loc should provide the free()
        }

        vfprintf(stderr, format, ap);
        fputc('\n', stderr);
    }

    void vwarningSupplemental(Loc loc, const(char)* format, va_list ap) { }

    void vdeprecation(Loc loc, const(char)* format, va_list ap)
    {
        fputs("Deprecation: ", stderr);
        const p = loc.toChars();
        if (*p)
        {
            fprintf(stderr, "%s: ", p);
            //mem.xfree(cast(void*)p); // loc should provide the free()
        }

        vfprintf(stderr, format, ap);
        fputc('\n', stderr);
    }

    void vmessage(Loc loc, const(char)* format, va_list ap)
    {
        const p = loc.toChars();
        if (*p)
        {
            fprintf(stderr, "%s: ", p);
            //mem.xfree(cast(void*)p); // loc should provide the free()
        }

        vfprintf(stderr, format, ap);
        fputc('\n', stderr);
    }

    void vdeprecationSupplemental(Loc loc, const(char)* format, va_list ap) { }
}

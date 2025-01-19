/**
Compilation time tracing, -ftime-trace.

The time trace profile is output in the Chrome Trace Event Format, described
here: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview

This file is originally from LDC (the LLVM D compiler).

Copyright: Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
Authors:   Johan Engelen, Max Haughton, Dennis Korpel
License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/common/timetrace.d, common/_timetrace.d)
Documentation: https://dlang.org/phobos/dmd_common_timetrace.html
Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/common/timetrace.d
*/
module dmd.timetrace;

import dmd.dsymbol;
import dmd.expression;

/**
 * Start a new time trace event
 *
 * Details of the event will be passed as delegates to `timeTraceEndEvent` so
 * they're only generated when the event is actually written.
 *
 * Params:
 *   eventType = what compilation stage the event belongs to
 *      (redundant with the eventType of `timeTraceEndEvent` but used by GDC)
 */
extern (C++)
void timeTraceBeginEvent(TimeTraceEventType eventType)
{
}

/**
 * End a time tracing event, optionally updating the event name and details
 * with a delegate. Delegates are used to prevent spending time on string
 * generation when an event is too small to be generated anyway.
 *
 * Params:
 *   eventType = what compilation stage the event belongs to
 *   sym = Dsymbol which was analyzed, used to generate 'name' and 'detail'
 *   e = Expression which was analyzed, used to generate 'name' and 'detail'
 *   detail = custom lazy string for 'detail' of event
 */
extern (C++)
void timeTraceEndEvent(TimeTraceEventType eventType)
{
}

/// ditto
void timeTraceEndEvent(TimeTraceEventType eventType, Dsymbol sym, scope const(char)[] delegate() detail = null)
{
    return timeTraceEndEvent(eventType);
}

/// ditto
extern (C++)
void timeTraceEndEvent(TimeTraceEventType eventType, Expression e)
{
    return timeTraceEndEvent(eventType);
}

/// Identifies which compilation stage the event is associated to
enum TimeTraceEventType
{
    generic,
    parseGeneral,
    parse,
    semaGeneral,
    sema1Import,
    sema1Module,
    sema2,
    sema3,
    ctfe,
    ctfeCall,
    codegenGlobal,
    codegenModule,
    codegenFunction,
    link,
}

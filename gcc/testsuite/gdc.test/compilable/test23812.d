// EXTRA_FILES: imports/imp23812.c

import imports.imp23812;

void callDefault()
{
    funcDefault();
    funcDefault2();
}

static assert(!__traits(compiles, () nothrow { funcDefault(); } ));
static assert(!__traits(compiles, () @nogc { funcDefault(); } ));
static assert(!__traits(compiles, () pure { funcDefault(); } ));

static assert(!__traits(compiles, () nothrow { funcDefault2(); } ));
static assert(!__traits(compiles, () @nogc { funcDefault2(); } ));
static assert(!__traits(compiles, () pure { funcDefault2(); } ));

void callNothrow() nothrow
{
    funcNothrow();
    funcNothrow2();
}

static assert(!__traits(compiles, () @nogc { funcNothrow(); } ));
static assert(!__traits(compiles, () pure { funcNothrow(); } ));

static assert(!__traits(compiles, () @nogc { funcNothrow2(); } ));
static assert(!__traits(compiles, () pure { funcNothrow2(); } ));

void callNogc() @nogc
{
    funcNogc();
}

static assert(!__traits(compiles, () nothrow { funcNogc(); } ));
static assert(!__traits(compiles, () pure { funcNogc(); } ));

void callPure() pure
{
    funcPure();
}

static assert(!__traits(compiles, () nothrow { funcPure(); } ));
static assert(!__traits(compiles, () @nogc { funcPure(); } ));

void callNothrowNogc() nothrow @nogc
{
    funcNothrowNogc();
    funcNothrowNogc2();
}

static assert(!__traits(compiles, () pure { funcNothrowNogc(); } ));

static assert(!__traits(compiles, () pure { funcNothrowNogc2(); } ));

extern(C) void callbackDefault()
{
}

extern(C) void callbackNothrow() nothrow
{
}

void callFuncWithCallback() nothrow
{
    funcWithCallback(&callbackNothrow);

    Callbacks callbacks;
    callbacks.f = &callbackNothrow;
}

static assert(!__traits(compiles, () { funcWithCallback(&callbackDefault); } ));

static assert(!__traits(compiles, () { Callbacks callbacks; callbacks.f = &callbackDefault; } ));

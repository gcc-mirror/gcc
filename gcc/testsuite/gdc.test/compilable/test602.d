// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

// Disallow skipping variable decl
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    int x;
    label: {}
    assert(!x);
}));

// Disallow skipping variable in block backwards
static assert(!__traits(compiles, (bool b)
{
    {
        int x;
        label: {}
        assert(!x);
    }
    if (b) goto label;
}));

// Disallow skipping backwards int block
static assert(!__traits(compiles, (bool b)
{
    {
    int x;
    label: {}
    assert(!x);
    }
    if (b) goto label;
}));

// Variable inside try block
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    try
    {
        int x;
    label: {}
        assert(!x);
    }
    catch
    {
    }
}));

// Variable inside catch block
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    try
    {
    }
    catch
    {
        int x;
    label: {}
        assert(!x);
    }
}));

// Goto into catch block with unnamed exception
static assert(__traits(compiles, (bool b)
{
    if (b) goto label;
    try
    {
    }
    catch(Exception)
    {
    label: {}
    }
}));

// Goto into catch block with named exception
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    try
    {
    }
    catch(Exception e)
    {
    label: {}
        assert(e);
    }
}));

// Goto into finally block
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    try
    {
    }
    finally
    {
    label: {}
    }
}));

// Goto into variable with block
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    struct S
    {
        int x;
    }
    with (S())
    {
    label: {}
        assert(!x);
    }
}));

// Goto backwards into variable with block
static assert(!__traits(compiles, (bool b)
{
    struct S
    {
        int x;
    }
    with (S())
    {
    label: {}
        assert(!x);
    }
    if (b) goto label;
}));

// Goto into symbolic with block
static assert(__traits(compiles, (bool b)
{
    if (b) goto label;
    struct S
    {
        int x;
    }
    with (S)
    {
    label: {}
    }
}));

// Goto backwards into symbolic with block
static assert(__traits(compiles, (bool b)
{
    struct S
    {
        int x;
    }
    with (S)
    {
    label: {}
    }
    if (b) goto label;
}));

// Goto into for loop
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    for (int i = 0; i < 8; ++i)
    {
    label: {}
        assert(i);
    }
}));

// Goto into for loop backwards
static assert(!__traits(compiles, (bool b)
{
    for (int i = 0; i < 8; ++i)
    {
    label: {}
        assert(i);
    }
    if (b) goto label;
}));

// Goto into foreach loop
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    foreach(i; 0..8)
    {
    label: {}
        assert(i);
    }
}));

// Goto into foreach loop backwards
static assert(!__traits(compiles, (bool b)
{
    foreach(i; 0..8)
    {
    label: {}
        assert(i);
    }
    if (b) goto label;
}));

// Goto into if block with variable
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    if (auto x = b)
    {
    label: {}
        assert(x);
    }
}));

// Goto backwards into if block with variable
static assert(!__traits(compiles, (bool b)
{
    if (auto x = b)
    {
    label: {}
        assert(x);
    }
    if (b) goto label;
}));

// Goto into if block without variable
static assert(__traits(compiles, (bool b)
{
    if (b) goto label;
    if (b)
    {
    label: {}
    }
}));

// Goto into else block
static assert(__traits(compiles, (bool b)
{
    if (b) goto label;
    if (auto x = b)
    {
    }
    else
    {
    label: {}
    }
}));

// Goto backwards into else with variable
static assert(!__traits(compiles, (bool b)
{
    if (auto x = b)
    {
    }
    else
    {
        int y;
    label: {}
    }
    if (b) goto label;
}));

// Goto into while block
static assert(__traits(compiles, (bool b)
{
    if (b) goto label;
    while (b)
    {
    label: {}
    }
}));

// Goto into while block with internal variable
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    while (b)
    {
        int x;
    label: {}
        assert(!x);
    }
}));

// Goto into do block
static assert(__traits(compiles, (bool b)
{
    if (b) goto label;
    do
    {
    label: {}
    }
    while (b);
}));

// Goto over switch variable
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    switch(0)
    {
    default:
        break;
        int x;
    label: {}
    }
}));

// Goto over switch variable
static assert(!__traits(compiles, (bool b)
{
    if (b) goto label;
    switch(0)
    {
    default:
        break;
    case 0:
        int x;
    label: {}
    }
}));

// Goto into synchronized statement
static assert(!__traits(compiles, (bool b)
{
    if (b)
        goto label;
    synchronized
    {
    label: {}
    }
}));

// Goto into scope(success) with variable
static assert(!__traits(compiles, (bool b)
{
    scope(success) { int x; label: {} assert(!x); }
    if (b)
        goto label;
}));

// Goto into scope(failure)
static assert(!__traits(compiles, (bool b)
{
    if (b)
        goto label;
    scope(failure) { label: {} }
}));

// Goto into scope(failure) with variable
static assert(!__traits(compiles, (bool b)
{
    scope(failure) { int x; label: {} assert(!x); }
    if (b)
        goto label;
}));

// Goto into scope(exit)
static assert(!__traits(compiles, (bool b)
{
    if (b)
        goto label;
    scope(exit) { label: {} }
}));

// Goto into scope(exit)
static assert(!__traits(compiles, (bool b)
{
    scope(exit) { label: {} }
    if (b)
        goto label;
}));

// Goto into scope(exit) with variable
static assert(!__traits(compiles, (bool b)
{
    scope(exit) { int x; label: {} assert(!x); }
    if (b)
        goto label;
}));

/***************************************************/
// 11659

int test11659()
{
    goto LABEL;
    enum expr = "0";
 LABEL:
    return mixin(expr);
}

/***************************************************/
// 13321

void test13321(bool b)
{
    static struct Foo
    {
        this(int) {}
    }

    Foo x;
    if (b)
        goto EXIT;
    x = Foo(1);
  EXIT:
}


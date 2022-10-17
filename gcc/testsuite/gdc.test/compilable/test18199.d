// https://issues.dlang.org/show_bug.cgi?id=18199

//
// struct initializer cases
//

// original error report
struct Bla
{
    int delegate(int, int) fun;
}
Bla bla1 = Bla((int a, int b) { return a + b; });
Bla bla2 = {(int a, int b) { return a + b; }};  // yielded error

// additional error report with memberName:expression syntax
struct Foo
{
    int function(int) bar;
    int function(int) bar2;
}
Foo foo =
{
    bar : function(x) { return 2 * x; },  // yielded error
    bar2 : (x) => 2 * x,
};

struct MyStruct
{
    int function() f;
    int delegate() d;
}

// confirm that ambiguous cases assume struct initializer
MyStruct ambiguous_1 = {};
MyStruct ambiguous_2 =
{
    { return 1 + 1; }
};

// statement-holding function literal variants not covered above
static MyStruct function_and_delegate_keywords =
{
    function () { return 1 + 1; },
    delegate () { return 1 + 1; }
};

//
// function literal initializer cases
//

alias IntFun = int function();
alias VoidFun = void function();

IntFun colon_at_top_level =
{
    return 1 + 1;
};

IntFun block_statement_only_with_nested_statement =
{
    if (true)
    {
        return 1 + 1;
    }
};

struct SomeStruct {}

// previously these cases were incorrectly parsed as struct initializer
VoidFun[] no_semicolon_statements = [
    { asm {} },
    { class Foo {} },
    { debug(foo) {} },
    { enum Foo { A } },
    { final switch(5) {} },
    { if (true) {} },
    { interface Foo {} },
    { pragma(inline) {} },
    { scope(exit) {} },
    { struct Foo {} },
    { synchronized {} },
    { try {} finally {} },
    { union Foo {} },
    { version(foo) {} },
    { while (false) {} },
    { with (SomeStruct) {} },
];

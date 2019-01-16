module test1673;

template Foo(T...) { }

template Bar(T...)
{
    template Doo(T...)
    {
    }
}

template Tuple(T...) { alias Tuple = T; }

void main()
{
    static assert( __traits(isTemplate, Foo));
    static assert(!__traits(isTemplate, Foo!int));
    static assert(!__traits(isTemplate, main));

    static assert( __traits(isTemplate, Bar));
    static assert(!__traits(isTemplate, Bar!int));
    static assert( __traits(isTemplate, Bar!(int).Doo));
    static assert(!__traits(isTemplate, Bar!(int).Doo!int));

    alias Tup = Tuple!(Foo, Foo!int, Bar, Bar!int, Bar!(int).Doo, Bar!(int).Doo!int);

    static assert( __traits(isTemplate, Tup[0]));
    static assert(!__traits(isTemplate, Tup[1]));
    static assert( __traits(isTemplate, Tup[2]));
    static assert(!__traits(isTemplate, Tup[3]));
    static assert( __traits(isTemplate, Tup[4]));
    static assert(!__traits(isTemplate, Tup[5]));
}

/// test overloads
void foo_over() { }
void foo_over(T : int)(T) { }
void foo_over(T : float)(T) { }
static assert(__traits(isTemplate, foo_over));

/// ditto
void bar_over() { }
void bar_over(int) { }
static assert(!__traits(isTemplate, bar_over));

/// alias to overloads
alias a_foo_over = foo_over;
static assert(__traits(isTemplate, a_foo_over));

/// ditto
alias a_bar_over = bar_over;
static assert(!__traits(isTemplate, a_bar_over));

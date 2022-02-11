/*
PERMUTE_ARGS:
*/

version (with_phobos) import std.conv : text;

struct Tuple(T...)
{
    T expand;
    alias expand this;
}

auto tuple(T...)(T t)
{
    return Tuple!T(t);
}

void fun0(U, T...)(U gold, int b_gold, T a, int b)
{
    assert(tuple(a) == gold);
    assert(b == b_gold);
}

void fun(U, T...)(U gold, T a, int b = 1)
{
    assert(tuple(a) == gold);
    assert(b == 1);
}

void fun2(U, V, T...)(U gold, V gold2, T a, string file = __FILE__, int line = __LINE__)
{
    assert(tuple(a) == gold);
    assert(tuple(file, line) == gold2);
}

void fun3(int[] gold, int[] a...)
{
    assert(gold == a);
}

void fun4(T...)(size_t length_gold, int b_gold, T a, int b = 1)
{
    assert(T.length == length_gold);
    assert(b == b_gold);
}

// Example in changelog
string log(T...)(T a, string file = __FILE__, int line = __LINE__)
{
    return text(file, ":", line, " ", a);
}

void fun_constraint(T...)(T a, string b = "bar") if (T.length == 1)
{
}

/+
NOTE: this is disallowed by the parser:
void fun5(int[] gold, int[] a ..., int b = 1)
{
    assert(gold==a);
    assert(b==1);
}
+/

void main()
{
    fun0(tuple(10), 7, 10, 7);

    fun(tuple());
    fun(tuple(10), 10);
    fun(tuple(10, 11), 10, 11);

    fun2(tuple(10), tuple(__FILE__, __LINE__), 10);

    fun3([1, 2, 3], 1, 2, 3);

    fun_constraint(1);
    assert(!__traits(compiles, fun_constraint(1, "baz")));

    version (with_phobos)
        assert(log(10, "abc") == text(__FILE__, ":", __LINE__, " 10abc"));

    // IFTI: `b` is always default-set
    fun4(0, 1);
    fun4(1, 1, 10);
    fun4(2, 1, 10, 11);

    // with explicit instantiation, and default-set `b`
    fun4!int(1, 1, 10);
    fun4!(int, int)(2, 1, 10, 11);

    // with explicit instantiation, and over-ridden `b`
    fun4!int(1, 100, 10, 100);
    fun4!(int, int)(2, 100, 10, 11, 100);

    // fun5([1,2,3], 1,2,3);
}

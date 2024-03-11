// https://issues.dlang.org/show_bug.cgi?id=11294

string result;

extern(C) void rt_finalize(void *ptr, bool det=true);
void clear(T)(T obj) if (is(T == class))
{
    rt_finalize(cast(void*)obj);
}

class A
{
    ~this() { result ~= "A"; }
    string dummy = "0";
}

class B
{
    A a;
    string dummy = "0";
    alias a this;
    ~this() { result ~= "B"; }
}

void test11294()
{
    auto a = new A;
    auto b = new B;
    b.a = a;
    result ~= b.dummy;
    clear(b);
    result ~= a.dummy;
    result ~= "END";
    clear(a);

    assert(result == "0B0ENDA");
}


// https://issues.dlang.org/show_bug.cgi?id=13392
void foo(T)(T t)
{
    void* p = cast(void*) t; //Callas alias this
}

class X {}

class Y
{
  alias a this;
  @property X a(){assert(0);} //Here
}

void test13392()
{
    foo(B.init);
}

void main()
{
    test11294();
    test13392();
}

/*
DISABLED: freebsd32 openbsd32 linux32 osx32 win32
TEST_OUTPUT:
---
fail_compilation/ice20042.d(18): Error: slice operation `cast(__vector(float[4]))[nanF, nanF, nanF, nanF] = [1.0F, 2.0F, 3.0F, 4.0F][0..4]` cannot be evaluated at compile time
fail_compilation/ice20042.d(25):        called from here: `Vec4(cast(__vector(float[4]))[nanF, nanF, nanF, nanF]).this([1.0F, 2.0F, 3.0F, 4.0F])`
---
*/
void write(T...)(T t){}

struct Vec4
{
    __vector(float[4]) raw;

    this(const(float[4]) value...) inout pure @safe nothrow @nogc
    {
        __vector(float[4]) raw;
        raw[] = value[];
        this.raw = raw;
    }
}

void main()
{
    static immutable Vec4 v = Vec4(  1.0f, 2.0f, 3.0f, 4.0f );

    static foreach(d; 0 .. 4)
        write(v.raw[d], " ");
}

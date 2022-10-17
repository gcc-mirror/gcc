/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/test20149.d(28): Error: escaping reference to stack allocated value returned by `S('\xff').this(1)`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20149#c10

@safe:

struct S
{
    this(int){ }

    char[] opSlice() return
    {
      return buf[];
    }

    char[4] buf;
}

S bar();

char[] fun()
{
    return S(1)[];
}

void main()
{
    auto x = fun();
}

// EXTRA_SOURCES: imports/ice10086y.d
// EXTRA_SOURCES: imports/ice10086x.d

import imports.ice10086y;
import imports.ice10086x;

void main() { test(); }

static if (0)
{
/* this is a reduced one-file version that triggers a seg fault
because the use of OPframeptr gets inlined, and the offests
to it are not updated.
Compile with: -O -inline
*/

pragma(inline, false)
auto bind(alias f, bindValues...)()
{
    pragma(inline, false)
    auto bind(Types...)(Types values)
    {
        return f(bindValues, values);
    }
    return bind();
}


struct SS
{
    int a1 = 123;
}

pragma(inline, false)
@safe auto ff(SS rr)
{
    return rr;
}

//   pragma(inline, false)
@safe auto gg(SS ss)  // this getting inlined triggers the problem
{
    return bind!(ff, ss);
}

pragma(inline, false)
void test()
{
    SS s1;

    auto zb = bind!(gg, s1)();
    assert(zb.a1 == 123);
}


void main() { test(); }
}

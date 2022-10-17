/* TEST_OUTPUT:
---
fail_compilation/test21546.d(113): Error: cannot implicitly convert expression `pc` of type `const(int)* delegate() return` to `int* delegate() return`
fail_compilation/test21546.d(114): Error: cannot implicitly convert expression `pc` of type `const(int)* delegate() return` to `immutable(int)* delegate() return`
fail_compilation/test21546.d(115): Error: cannot implicitly convert expression `pi` of type `immutable(int)* delegate() return` to `int* delegate() return`
fail_compilation/test21546.d(213): Error: cannot implicitly convert expression `dc` of type `const(int) delegate() ref return` to `int delegate() ref return`
fail_compilation/test21546.d(214): Error: cannot implicitly convert expression `dc` of type `const(int) delegate() ref return` to `immutable(int) delegate() ref return`
fail_compilation/test21546.d(215): Error: cannot implicitly convert expression `di` of type `immutable(int) delegate() ref return` to `int delegate() ref return`
fail_compilation/test21546.d(305): Error: cannot implicitly convert expression `[dgi]` of type `immutable(int) delegate() ref return[]` to `int delegate() ref return[]`
---
 */
// https://issues.dlang.org/show_bug.cgi?id=21546

#line 100

alias Pm =           int*  delegate() return;
alias Pc =     const(int)* delegate() return;
alias Pi = immutable(int)* delegate() return;

void f()
{
    Pm pm;
    Pc pc;
    Pi pi;
    pc = pm;
    pc = pi;

    pm = pc;
    pi = pc;
    pm = pi;
}

#line 200

alias DGm = ref           int  delegate() return;
alias DGc = ref     const(int) delegate() return;
alias DGi = ref immutable(int) delegate() return;

void g()
{
    DGm dm;
    DGc dc;
    DGi di;
    dc = dm;
    dc = di;

    dm = dc;
    di = dc;
    dm = di;
}

#line 300

void h()
{
    immutable int i = 0;
    DGi dgi = ref() => i;
    DGm[] dgms = [ dgi ];
}

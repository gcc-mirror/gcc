// https://bugzilla.gdcproject.org/show_bug.cgi?id=179
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

import core.stdc.stdio;

struct S179a
{
    @disable this(this);
}

struct S179b
{
    S179a s1;
    void connect() { printf("this=%p\n", &this); }
}

class C179
{
    private S179b s2;
    ref S179b value() @property
    {
        printf("this=%p\n", &s2);
        return s2;
    }
}

void main()
{
    C179 a = new C179;
    a.value.connect();
}

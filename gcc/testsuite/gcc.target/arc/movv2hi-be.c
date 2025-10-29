/* { dg-do run } */
/* { dg-options "-O2" } */

typedef short v2hi __attribute__((vector_size(4)));

__attribute__((noinline)) void foo3(short a)
{
    if (a != 520)
    {
        __builtin_abort();
    }
}

__attribute__((noinline)) void foo2(v2hi v)
{
    foo3(v[0]);
}

__attribute__((noinline)) void foo(v2hi *v)
{
    foo2(*v);
}

int main (void)
{
    v2hi v;
    v[0] = 520;
    v[1] = -1;
    foo(&v);
    foo2(v);
    return 0;
}

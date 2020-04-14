// { dg-do compile }
// { dg-options "-O2 -fno-inline" }

import gcc.attributes;

extern int t();

int func()
{
    __gshared int var = 42;
    int nested_1() @no_icf
    {
        return var++;
    }
    int nested_2()
    {
        return var++;
    }
    int nested_3()
    {
        return var++;
    }
    return nested_1() + nested_2() + nested_3();
}

@no_icf int var = 0; // { dg-warning ".no_icf. attribute ignored" }

// { dg-final { scan-assembler "nested_1" } }
// { dg-final { scan-assembler "nested_2" } }
// { dg-final { scan-assembler-not "nested_3" } }

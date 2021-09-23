// { dg-do compile }
// { dg-options "-O2 -finline-functions -fno-ipa-icf" }

import gcc.attributes;

extern int t();

void func()
{
    void nested_function() @noinline
    {
        t();
    }
    nested_function();
}

@noinline int var = 0; // { dg-warning ".noinline. attribute ignored" }

// { dg-final { scan-assembler "nested_function" } }

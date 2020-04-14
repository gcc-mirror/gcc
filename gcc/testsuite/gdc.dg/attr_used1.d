// { dg-do compile }
// { dg-options "-O2" }

import gcc.attributes;

int func()
{
    int nested_function() @used
    {
        return 1;
    }
    @used int var = 0; // { dg-warning ".used. attribute ignored" }
    return var;
}

// { dg-final { scan-assembler "nested_function" } }

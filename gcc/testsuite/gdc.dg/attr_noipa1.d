// { dg-do compile }
// { dg-options "-O2 -finline-functions" }

import gcc.attributes;

int func(int x)
{
    int nested_function(int y, int z) @noipa
    {
        return y + z;
    }
    return nested_function(x, 0);
}

@noipa int var = 0; // { dg-warning ".noipa. attribute ignored" }

// { dg-final { scan-assembler "nested_function" } }

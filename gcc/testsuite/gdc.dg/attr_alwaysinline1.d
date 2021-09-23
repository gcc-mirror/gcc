// { dg-do compile }
// { dg-options "-O0" }

import gcc.attributes;

int func()
{
    int nested_function() @always_inline
    {
        return 13;
    }
    return nested_function();
}

@always_inline int var = 0; // { dg-warning ".always_inline. attribute ignored" }

// { dg-final { scan-assembler-not "nested_function" } }

// { dg-do compile }
// { dg-options "-O1" }

import gcc.attributes;

int func() @flatten
{
    __gshared int count = 0;
    int nested_function()
    {
        return count++;
    }
    static foreach (_; 0 .. 1000)
        nested_function();

    return nested_function();
}

@flatten int var = 0; // { dg-warning ".flatten. attribute ignored" }

// { dg-final { scan-assembler-not "nested_function" } }

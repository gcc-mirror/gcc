// { dg-do compile { target x86_64-*-linux* } }
// { dg-options "-O2 -fno-pic" }

import gcc.attributes;

@noplt int func();

@noplt int var = 0; // { dg-warning ".noplt. attribute ignored" }

int main()
{
    return func();
}

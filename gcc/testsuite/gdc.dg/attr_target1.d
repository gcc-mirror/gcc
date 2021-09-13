// { dg-do compile { target i?86*-*-* x86_64-*-* } }

import gcc.attributes;

@target("default")
int foo() { return 1; }

@target("arch=core2", "")
int foo2() { return 2; } // { dg-warning "empty string in attribute .target." }

@target("sse4.2", "", "")
int foo3() { return 3; } // { dg-warning "empty string in attribute .target." }

@target("default")
int var = 0; // { dg-warning ".target. attribute ignored" }

int main()
{
    return foo() + foo2() + foo3();
}

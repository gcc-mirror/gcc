// { dg-do compile { target i?86*-*-* x86_64-*-* } }

import gcc.attributes;

@target_clones("default")
int func() // { dg-warning "single .target_clones. attribute is ignored" }
{
    return 0;
}

@target_clones("default")
int var = 0; // { dg-warning ".target_clones. attribute ignored" }

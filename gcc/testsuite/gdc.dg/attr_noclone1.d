/* { dg-do compile } */
/* { dg-options "-O2" } */

import gcc.attributes;

@target_clones("avx", "default")
@noclone
void func() // { dg-error "clones for .target_clones. attribute cannot be created" }
{           // { dg-message "function .func. can never be copied because it has .noclone. attribute" "" { target *-*-* } .-1 }
}

@noclone int var = 0; // { dg-warning ".noclone. attribute ignored" }

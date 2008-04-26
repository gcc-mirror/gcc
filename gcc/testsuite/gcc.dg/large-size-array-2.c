/* PR c/25309 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
static char * name[] = {
    [0x80000000]  = "bar"
  };
/* { dg-error "too large" "" { target { { ! lp64 } || x86_64-*-mingw* } }  6 } */

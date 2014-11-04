/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O3 -mcpu=power7" } */

/* The bug shows up if you compile with -maltivec or -mcpu=power7, due to one
   of the vector's being eliminated due to rs6000_macro_to_expand being called
   recursively.  */

struct vector {
  float v[4];
};

struct vector vector = { 1.0, 2.0, 3.0, 4.0 };

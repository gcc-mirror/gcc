/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mabi_ilp32 } */
/* { dg-options "-mabi=ilp32 -Wno-deprecated" } */

void f (void *b) 
{ 
  __builtin_update_setjmp_buf (b); 
}


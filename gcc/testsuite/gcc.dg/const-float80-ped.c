/* Test 'w' suffix with -pedantic on __float80 type constants.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "-pedantic" } */
/* { dg-options "-mmmx -pedantic" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

__float80 a = 123.456789w;  /* { dg-warning "non-standard suffix on floating constant" } */

/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -fshrink-wrap -fdump-rtl-pro_and_epilogue -fno-stack-protector" } */
/* { dg-additional-options "-mno-avx" { target ia32 } } */
/* { dg-skip-if "No shrink-wrapping preformed" { x86_64-*-mingw* } } */

#include <string.h>

int c;
int x[2000];
__attribute__((regparm(1))) void foo (int a, int b)
 {
   int t[200];
   if (a == 0 || c == 0)
     return;
   memcpy (t, x + b, sizeof t);
   c = t[a];
 }

/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" } } */

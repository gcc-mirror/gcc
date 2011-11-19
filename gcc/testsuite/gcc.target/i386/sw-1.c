/* { dg-do compile } */
/* { dg-options "-O2 -fshrink-wrap -fdump-rtl-pro_and_epilogue -mstringop-strategy=rep_byte" } */

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
/* { dg-final { cleanup-rtl-dump "pro_and_epilogue" } } */

/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-22.i "^\n*#undef AAA\n+AAA is undefined\n+#undef BBB\n+BBB is undefined\n+#undef CCC\n+CCC is undefined\n*$" } } */
#ifndef AAA
AAA is undefined
#endif

#ifndef BBB
BBB is undefined
#endif

#ifndef CCC
CCC is undefined
#endif

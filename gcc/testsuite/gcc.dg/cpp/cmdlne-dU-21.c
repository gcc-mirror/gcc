/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-21.i "^\n*hello There\n+#define ASTRING There\n+#define MACROARGS\\(A\\) A\n+#undef BSTRING\n*$" } } */
#define ASTRING There
#define MACROARGS(A) A
MACROARGS(hello) ASTRING
#ifdef BSTRING
bye
#endif

/* From PR37280.  */
/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "-fno-common -Os" } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?kallsyms_token_index" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?kallsyms_token_table" } } */
/* { dg-skip-if "" { x86_64-*-mingw* } } */
/* NVPTX's weak is applied to the definition,  not declaration.  */
/* { dg-skip-if "" { nvptx-*-* } } */

extern int kallsyms_token_index[] __attribute__((weak));
extern int kallsyms_token_table[] __attribute__((weak));
void kallsyms_expand_symbol(int *result)
{
  int len = *result;
  int *tptr;
  while(len) {
    tptr = &kallsyms_token_table[ kallsyms_token_index[*result] ];
    len--;
    while (*tptr) tptr++;
    *tptr = 1;
  }
 *result = 0;
}

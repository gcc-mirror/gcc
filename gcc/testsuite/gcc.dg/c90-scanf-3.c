/* Test for scanf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

typedef __WCHAR_TYPE__ wchar_t;

extern int scanf (const char *, ...);

void
foo (char **sp, wchar_t **lsp)
{
  /* %a formats for allocation, only recognised in C90 mode, are a
     GNU extension.
  */
  scanf ("%as", sp); /* { dg-warning "C" "%as" } */
  scanf ("%aS", lsp); /* { dg-warning "C" "%aS" } */
  scanf ("%a[bcd]", sp); /* { dg-warning "C" "%a[]" } */
}

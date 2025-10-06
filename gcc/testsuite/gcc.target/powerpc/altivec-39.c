/* { dg-do compile } */
/* { dg-options "-O2 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

char *sort_filesquote_name_buf_plimit;
int sort_filesquote_name_buf_width;
int *__ctype_b_loc() __attribute__((__const__));
void sort_filesquote_name_buf(char* p)
{
  unsigned displayed_width = 0;
  while (p < sort_filesquote_name_buf_plimit) {
    if (__ctype_b_loc()[*p])
      displayed_width++;
    p++;
  }
  sort_filesquote_name_buf_width = displayed_width;
}

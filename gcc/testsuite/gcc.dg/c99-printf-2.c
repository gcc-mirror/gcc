/* Test for printf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

typedef __SIZE_TYPE__ size_t;
typedef __WCHAR_TYPE__ wchar_t;
typedef __WINT_TYPE__ wint_t;

extern int printf (const char *, ...);

void
foo (int i, long long ll, size_t z, wint_t lc, wchar_t *ls)
{
  /* The length modifiers q, Z and L as applied to integer formats are
     extensions.
  */
  printf ("%qd", ll); /* { dg-warning "C" "%q length" } */
  printf ("%Ld", ll); /* { dg-warning "C" "%L length" } */
  printf ("%Zd", z); /* { dg-warning "C" "%Z length" } */
  /* The conversion specifiers C and S are X/Open extensions; the
     conversion specifier m is a GNU extension.
  */
  printf ("%m"); /* { dg-warning "C" "printf %m" } */
  printf ("%C", lc); /* { dg-warning "C" "printf %C" } */
  printf ("%S", ls); /* { dg-warning "C" "printf %S" } */
  /* The flag character ', and the use of operand number $ formats, are
     X/Open extensions.
  */
  printf ("%'d", i); /* { dg-warning "C" "printf ' flag" } */
  printf ("%1$d", i); /* { dg-warning "C" "printf $ format" } */
}

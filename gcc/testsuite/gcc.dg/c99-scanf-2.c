/* Test for scanf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

typedef __SIZE_TYPE__ size_t;
typedef __WCHAR_TYPE__ wchar_t;

extern int scanf (const char *, ...);

void
foo (int *ip, long long int *llp, size_t *zp, wchar_t *ls)
{
  /* The length modifiers q, Z and L as applied to integer formats are
     extensions.
  */
  scanf ("%qd", llp); /* { dg-warning "C" "%q length" } */
  scanf ("%Ld", llp); /* { dg-warning "C" "%L length" } */
  scanf ("%Zu", zp); /* { dg-warning "C" "%Z length" } */
  /* The conversion specifiers C and S are X/Open extensions.  */
  scanf ("%C", ls); /* { dg-warning "C" "scanf %C" } */
  scanf ("%S", ls); /* { dg-warning "C" "scanf %S" } */
  /* The use of operand number $ formats is an X/Open extension.  */
  scanf ("%1$d", ip); /* { dg-warning "C" "scanf $ format" } */
}

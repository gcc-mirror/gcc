/* PR c++/25294 */
/* { dg-options "-std=gnu99" } */
/* Epiphany makes struct S 8-byte aligned.  */
/* { dg-do run { target { ! epiphany-*-* } } } */

extern void abort (void);

struct S
{
  char a[3];
#pragma pack(1) /* A block comment
		   that ends on the next line.  */
  struct T
  {
    char b;
    int c;
  } d;
#pragma pack /*/ */ () // C++ comment
  int e;
} s;

int
main ()
{
  if (sizeof (int) == 4 && sizeof (s) != 12)
    abort ();
  return 0;
}

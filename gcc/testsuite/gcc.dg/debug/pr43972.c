/* PR debug/43972 */
/* { dg-do compile } */
/* { dg-options "-g -w" } */
/* { dg-options "-g -fpic -w" { target fpic } } */

struct { int *b1; } *f1 ();
short v1[1];
struct S { int b2; };
void
foo (struct S *a1, union { char *b3; unsigned *b4; int *b5; } *a2)
{
  int d;
  switch (d)
    {
    case 0:
      {
	int c = a1->b2, i;
	if (f1 () == 0)
	  *a2->b3++ = 2;
	else if (((long) (f1 () - f1 ())) ^ ((long) f1 ()->b1 - ((long) f1 () & 8)))
	  *a2->b3++ = (long) f1 - ((long) f1 () & 0xff);
	else
	  *a2->b4++ = (long) f1;
	for (i = 0; i < c; i++)
	  *a2->b5++ = (long) v1;
	foo (a1, a2);
      }
    }
}

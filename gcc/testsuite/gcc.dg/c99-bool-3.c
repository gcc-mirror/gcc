/* Test for _Bool bit-fields.  They have the semantics of _Bool, at
   least for now (DR#335 Spring 2007 discussion).  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */
struct foo
{
  _Bool a : 1;
} sf;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int i;
  for (i = 0; i < sizeof (struct foo); i++)
    *((unsigned char *)&sf + i) = (unsigned char) -1;
  sf.a = 2;
  if (sf.a != 1)
    abort ();
  sf.a = 0;
  if (sf.a != 0)
    abort ();
  sf.a = 0.2;
  if (sf.a != 1)
    abort ();
  sf.a = &sf;
  if (sf.a != 1)
    abort ();
  exit (0);
}

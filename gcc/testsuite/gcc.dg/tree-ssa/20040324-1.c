/* { dg-do run } */
/* { dg-options "-O2" } */

/* Ensure that BIT_FIELD_REFs gets the appropriate VUSE.
   Contributed by Paolo Bonzini  <bonzini@gnu.org>.

   This testcase actually never triggered in the CVS repo, but it did
   in my local tree and it seems worth testing.  In this test, the if's
   are folded to BIT_FIELD_REFs but the VUSEs were erroneously left out.
   Therefore, DOM did not see that i was modified between the two ifs
   and optimized away the second if.  */

extern void abort (void);
extern void exit (int);

struct x
{
  unsigned b:1;
  unsigned c:1;
};

struct x i = { 1, 1 };

int
main ()
{
  i.b = 1;
  if (i.b == 1 && i.c == 0)
    exit (0);
  i.c = 0;
  if (i.b == 1 && i.c == 0)
    exit (0);
  abort ();
}

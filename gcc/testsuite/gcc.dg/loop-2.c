/* PR optimization/10171 */
/* Bug: unroll_loop misoptimized the function so that we got
   0 iterations of the loop rather than the correct 1.  */
/* { dg-do run } */

extern void abort (void);
extern void exit (int);

__inline__ int tag() { return 0; }

void f ();

int main() {
  int i;
  for (i = 0; i < (tag() ? 2 : 1); i++)
    f();
  abort ();
}

void f ()
{
  exit (0);
}

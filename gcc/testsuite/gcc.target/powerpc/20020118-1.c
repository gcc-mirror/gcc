/* { dg-do run { target powerpc*-*-* } }*/

/* Test local alignment.  Test new target macro STARTING_FRAME_PHASE.  */
/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */

extern void abort(void);

int main ()
{       
  int darisa[4] __attribute__((aligned(16))) ;
  int *stephanie = (int *) darisa;

  if ((unsigned long) stephanie % 16 != 0)
    abort ();

  return 0;
}

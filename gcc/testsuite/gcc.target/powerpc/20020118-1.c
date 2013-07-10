/* { dg-do run { target powerpc*-*-* } }*/
/* VxWorks only guarantees 64 bits of alignment (STACK_BOUNDARY == 64).  */
/* { dg-skip-if "" { "powerpc*-*-vxworks*" } { "*" } { "" } } */
/* Force 128-bit stack alignment for eabi targets.  */
/* { dg-options "-mno-eabi" { target powerpc*-*-eabi* } } */

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

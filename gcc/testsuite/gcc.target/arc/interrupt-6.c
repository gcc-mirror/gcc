/* { dg-do compile } */
/* { dg-skip-if "Not available for ARCv1" { arc700 || arc6xx } } */
/* { dg-options "-O2 -mirq-ctrl-saved=r0-ilink" } */

#include <alloca.h>

/* Check if ilink is recognized. Check how FP and BLINK are saved.
   BLINK is saved last on the stack because the IRQ autosave will do
   first r0-ilink.  To avoid this ABI exception, one needs to autosave
   always blink when using the IRQ autosave feature.  */

extern int bar (void *);

void  __attribute__ ((interrupt("ilink")))
foo(void)
{
  int *p = alloca (10);
  bar (p);
}
/* { dg-final { scan-assembler-not ".*fp,\\\[sp" } } */
/* { dg-final { scan-assembler "ld.*blink,\\\[sp" } } */
/* { dg-final { scan-assembler "push_s.*blink" } } */

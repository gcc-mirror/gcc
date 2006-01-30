/* { dg-do compile { target sh-*-* sh[1234ble]*-*-*} } */
/* { dg-options "-O" } */
extern void foo ();

void
(__attribute__ ((trapa_handler)) isr) ()
{
  foo ();
}

/* { dg-final { scan-assembler-times "rte" 1} } */
/* No interrupt-specific saves should be needed.  /
/* { dg-final { scan-assembler-not "\[^f\]r\[0-7\]\[ \t,\]\[^\n\]*r15" } } */
/* { dg-final { scan-assembler-not "@r15\[^\n\]*\[^f\]r\[0-7\]\n" } } */
/* { dg-final { scan-assembler-not "\[^f\]r\[8-9\]" } } */
/* { dg-final { scan-assembler-not "\[^f\]r1\[,0-3\]" } } */
/* { dg-final { scan-assembler-not "macl" } } */

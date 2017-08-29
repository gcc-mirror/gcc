/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt -funroll-loops" } */

extern void *f();
void dmi_scan_machine(void) {
  char *p = f(), *q;
  for (q = p; q < p + 10; q++)
    ;
}

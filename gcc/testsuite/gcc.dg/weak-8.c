/* { dg-do assemble } */

/* COFF does not support weak, and dg doesn't support UNSUPPORTED.  */
/* { dg-do assemble { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */

__attribute__ ((weak)) int i;

int f() {
  return i;
}

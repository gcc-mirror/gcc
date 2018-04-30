/* PR81534 This testcase used to fail because the HI/QI
   "atomic_fetch_<atomic><mode>" expander accepted symbolic references
   and emitted CAS patterns whose insn definition rejected them.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=zEC12" } */

struct {
  short b;
  long c;
} a = {};

void
d ()
{
  __atomic_fetch_add(&a.b, 0, 5);
}

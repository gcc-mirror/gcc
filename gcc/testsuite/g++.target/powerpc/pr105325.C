/* { dg-do assemble } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -fstack-protector" } */

/* PR target/105324.  Test that power10 fusion does not generate an LWA/CMPDI
   with a large offset that the assembler rejects.  Instead it should a
   PLWZ/CMPWI combination.

   Originally, the code was dying because the fusion load + compare -1/0/1
   patterns did not handle the possibility that the load might be prefixed.
   The -fstack-protector option is needed to show the bug.  */

struct Ath__array1D {
  int _current;
  int getCnt() { return _current; }
};
struct extMeasure {
  int _mapTable[10000];
  Ath__array1D _metRCTable;
};
void measureRC() {
  extMeasure m;
  for (; m._metRCTable.getCnt();)
    for (;;)
      ;
}

/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;
extern int j;

CDTOR_LINKAGE
void d3() __attribute__((destructor (600)));

CDTOR_LINKAGE
void d3() {
  if (j != 2)
    __builtin_abort ();
  if (--i != 1)
    __builtin_abort ();
}

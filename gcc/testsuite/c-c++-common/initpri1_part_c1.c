/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;

CDTOR_LINKAGE
void c1() __attribute__((constructor (500)));

CDTOR_LINKAGE
void c1() {
  if (i++ != 0)
    __builtin_abort ();
}

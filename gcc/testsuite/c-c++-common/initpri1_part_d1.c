/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;

CDTOR_LINKAGE
void d1() __attribute__((destructor (500)));

CDTOR_LINKAGE
void d1() {
  if (--i != 0)
    __builtin_abort ();
}

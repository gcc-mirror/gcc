/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;

CDTOR_LINKAGE
void d2() __attribute__((destructor (700)));

CDTOR_LINKAGE
void d2() {
  if (--i != 2)
    __builtin_abort ();
}

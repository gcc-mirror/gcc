/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;
extern int j;

CDTOR_LINKAGE
void cd4() __attribute__((constructor (800), destructor (800)));

CDTOR_LINKAGE
void cd4() {
  if (i != 3)
    __builtin_abort ();
  ++j;
}

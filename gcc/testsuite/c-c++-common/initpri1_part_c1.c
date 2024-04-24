/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;

void c1() __attribute__((constructor (500)));

void c1() {
  if (i++ != 0)
    __builtin_abort ();
}

/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;

void c3() __attribute__((constructor (600)));

void c3() {
  if (i++ != 1)
    __builtin_abort ();
}

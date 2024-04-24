/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

extern int i;

void c2() __attribute__((constructor (700)));

void c2() {
  if (i++ != 2)
    __builtin_abort ();
}

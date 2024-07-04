/* { dg-skip-if part { *-*-* } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

int i;
int j;

int main () {
  if (i != 3)
    return 1;
  if (j != 1)
    __builtin_abort ();
  return 0;
}

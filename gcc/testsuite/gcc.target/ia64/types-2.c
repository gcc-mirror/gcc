/* { dg-do run { target ia64*-hp-hpux* } } */
/* { dg-options } */

/* Test that the sizes and alignments of the extra floating-point
   types are correct.  */

int main () {
  if (sizeof (__fpreg) != 16)
    return 1;
  if (__alignof__ (__fpreg) != 16)
    return 2;

  if (sizeof (__float80) != 16)
    return 3;
  if (__alignof__ (__float80) != 16)
    return 4;

  return 0;
}


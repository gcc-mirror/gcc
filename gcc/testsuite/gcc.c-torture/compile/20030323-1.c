/* PR c/10178.  The following code would ICE because we didn't check for
   overflow when computing the range of the switch-statment, and therefore
   decided it could be implemented using bit-tests.  */

int
banana(long citron)
{
  switch (citron) {
    case 0x80000000:
    case 0x40000:
    case 0x40001:
      return 1;
      break;
  }
  return 0;
}


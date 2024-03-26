// { dg-do compile }

void h(unsigned char *scratch, bool carry)
{
  for (int i = 0; i < 16; i++) {
    bool b = scratch[i] <<= 1;
    if (carry)
      scratch[i] |= 1;
    carry = b;
  }
}

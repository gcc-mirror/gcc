static __inline__ unsigned char BCD(unsigned char binval)
{
  if (binval > 99) return 0x99;
  return (((binval/10) << 4) | (binval%10));
}

void int1a(unsigned char i)
{
    (*((unsigned char *)1)) = BCD(i);
}

foo (b, c)
     unsigned b, c;
{
  return (b << 12) | (c >> 20);
}

main ()
{
  printf ("0x%x\n", foo (0x11223344, 0xaabbccdd));
}

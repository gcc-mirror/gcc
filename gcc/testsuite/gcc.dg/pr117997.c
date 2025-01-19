/* PR tree-optimization/117997 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fpie" { target pie } } */

__attribute__((noipa)) unsigned char
foo (const unsigned char *data, unsigned int len)
{
  __builtin_abort ();
}

__attribute__((noipa)) unsigned short
bar (const unsigned char *data, unsigned int len)
{
  __builtin_abort ();
}

static unsigned char
baz (unsigned char byte, unsigned char crc)
{
  unsigned int i;
  crc ^= byte;
  for (i = 0; i < 8; i++)
    crc = (crc << 1) ^ ((crc >> 7) ? 0x07 : 0);
  return crc;
}

static unsigned short
qux (unsigned char byte, unsigned short crc)
{
  unsigned int i;
  crc ^= byte << 8;
  for (i = 0; i < 8; i++)
    crc = (crc << 1) ^ ((crc >> 15) ? 0x8005 : 0);
  return crc;
}

__attribute__((noipa)) int
corge (const unsigned char *data)
{
  const unsigned char expected[] = {
    0x00, 0x07, 0x1b, 0x48, 0xe3, 0xbc, 0x2f, 0xd8,
    0x3e, 0x85, 0xa4, 0x44, 0xff, 0xd0, 0x14, 0x41,
    0xb0, 0x6e, 0x73, 0x27, 0x99, 0xad, 0x28, 0xbd,
    0x72, 0x16, 0x24, 0xbd, 0x6e, 0x5e, 0xc7, 0x06
  };
  for (int i = 0; i < 32; ++i)
    if (data[i] != expected[i])
      __builtin_abort ();
  return 1;
}

static int
garply (const unsigned char *data, unsigned long size)
{
  unsigned int i;
  unsigned char crc0, crc1;
  crc0 = 0;
  crc1 = foo (data, 0);
  if (crc1 != crc0)
    return 0;
  for (i = 0; i < size; i++)
    {
      crc0 = baz (data[i], crc0);
      crc1 = foo (data, i + 1);
      if (crc1 != crc0)
	return 0;
    }
  return 1;
}

static int
freddy (const unsigned char *data, unsigned long size)
{
  unsigned int i;
  unsigned short crc0, crc1;
  crc0 = 0;
  crc1 = bar (data, 0);
  if (crc1 != crc0)
    return 0;
  for (i = 0; i < size; i++)
    {
      crc0 = qux (data[i], crc0);
      crc1 = bar (data, i + 1);
      if (crc1 != crc0)
	return 0;
    }
  return 1;
}

__attribute__((noipa)) int
blah (void)
{
  unsigned int i;
  unsigned char data[64] = { 0 };
  for (i = 1; i < 64; i++)
    data[i] = baz (i % 256, data[i - 1]);
  if (corge (data))
    return 0;
  if (!garply (data, 64))
    return 1;
  if (!freddy (data, 64))
    return 1;
   return 1;
}

int
main ()
{
  if (__CHAR_BIT__ == 8 && __SIZEOF_SHORT__ == 2 && blah ())
    __builtin_abort ();
}

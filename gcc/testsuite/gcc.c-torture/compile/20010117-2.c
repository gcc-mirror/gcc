unsigned char a, b;

void baz (void)
{
  if (b & 0x08)
    {
      int g = 0;
      int c = (b & 0x01);
      int d = a - g - c;
      int e = (a & 0x0f) - (g & 0x0f);
      int f = (a & 0xf0) - (g & 0xf0);
      int h = (a & 0x0f) - (g & 0x0f);

      if ((a ^ g) & (a ^ d) & 0x80) b |= 0x40;
      if ((d & 0xff00) == 0) b |= 0x01;
      if (!((a - h - c) & 0xff)) b |= 0x02;
      if ((a - g - c) & 0x80) b |= 0x80;
      a = (e & 0x0f) | (f & 0xf0);
    }
}

unsigned char u, v, w;

void baz (void)
{
  if ((u - v - w) & 0x80)
    v = 1;
}

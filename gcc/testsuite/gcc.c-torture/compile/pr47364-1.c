static unsigned char foo[256];

void
arc4_init(void)
{
  int n;

  for (n = 0; n < 256; n++)
    foo[n] = n;
}

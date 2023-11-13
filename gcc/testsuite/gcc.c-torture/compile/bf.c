typedef unsigned long uint32;
typedef signed long sint32;
int clr (int, int, int);
int atoi (const char *);

uint32
ext (sint32 src, unsigned o5, unsigned w5)
{
  return (w5 == 0) ? src >> o5 : (src << (( - o5 - w5) & 31)) >> (32 - w5);
}

uint32
extu (uint32 src, unsigned o5, unsigned w5)
{
  return (w5 == 0) ? src >> o5 : (src << (( - o5 - w5) & 31)) >> (32 - w5);
}

uint32
mak (uint32 src, unsigned o5, unsigned w5)
{
  return (w5 == 0) ? src << o5 : (src << (32 - w5)) >> (( - o5 - w5) & 31);
}

uint32
rot (uint32 src, unsigned o5)
{
  return (src >> o5) | (src << (( - o5) & 31));
}

int
main (int argc, char **argv)
{
  __builtin_printf ("%x\n", clr (0xffffffff, atoi (argv[2]), atoi (argv[1])));
}

#ifdef __SIZEOF_INT128__
typedef __int128 L;
#else
typedef long long L;
#endif
float f;

int
main ()
{
  L i = f;
  if (i <= 10)
    do
      {
	++i;
	asm ("");
      }
    while (i != 11);
  return 0;
}

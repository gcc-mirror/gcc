/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O" } */

typedef signed long long int S;
typedef unsigned long long int U;
typedef __int128 W;
__attribute__ ((noinline, noclone))
U upseu (U x, S y, int *ovf)
{
  U res;
  *ovf = __builtin_add_overflow (x, y, &res);
  return res;
}
U
usueu (U x, U y, int *ovf)
{
  U res;
  *ovf = __builtin_sub_overflow (x, y, &res);
  return res;
}
U
usseu (U x, S y, int *ovf)
{
  U res;
  *ovf = __builtin_sub_overflow (x, y, &res);
  return res;
}
int
main ()
{
  int i, j;
  for (i = 0; i < ((unsigned char) ~0); i++)
    for (j = 0; j < ((unsigned char) ~0); j++)
      {
	U u1 = ((W) i << ((8 - 1) * 8));
	S s2 = ((W) j << ((8 - 1) * 8)) + (-0x7fffffffffffffffLL - 1);
	U u2 = ((W) j << ((8 - 1) * 8));
	W w;
	int ovf;
	w = ((W) u1) + ((W) s2);
	if (upseu (u1, s2, &ovf) != (U) w || ovf != (w != (U) w))
	  __builtin_abort ();
	w = ((W) u1) - ((W) u2);
	if (usueu (u1, u2, &ovf) != (U) w || ovf != (w != (U) w))
	  __builtin_abort ();
	w = ((W) u1) - ((W) s2);
	if (usseu (u1, s2, &ovf) != (U) w || ovf != (w != (U) w))
	  __builtin_abort ();
      }
}

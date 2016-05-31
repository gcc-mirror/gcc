/* PR target/71056.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_vfp3_ok } */
/* { dg-options "-O3 -mfpu=vfpv3" } */

/* Check that compiling for a non-NEON target doesn't try to introduce
   a NEON vectorized builtin.  */

extern char *buff;
int f2 ();
struct T1
{
  int reserved[2];
  unsigned int ip;
  unsigned short cs;
  unsigned short rsrv2;
};
void
f3 (const char *p)
{
  struct T1 x;
  __builtin_memcpy (&x, p, sizeof (struct T1));
  x.reserved[0] = __builtin_bswap32 (x.reserved[0]);
  x.reserved[1] = __builtin_bswap32 (x.reserved[1]);
  x.ip = __builtin_bswap32 (x.ip);
  x.cs = x.cs << 8 | x.cs >> 8;
  x.rsrv2 = x.rsrv2 << 8 | x.rsrv2 >> 8;
  if (f2 ())
    {
      __builtin_memcpy (buff, "\n", 1);
    }
}

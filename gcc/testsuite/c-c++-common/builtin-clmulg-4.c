/* Test __builtin_clmulg.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

[[gnu::noipa]] unsigned char
clmul8 (unsigned char x)
{
  return __builtin_clmulg (x, x);
}

[[gnu::noipa]] unsigned char
clmul8w (unsigned char x, unsigned char *y)
{
  return __builtin_clmulg (x, x, y);
}

[[gnu::noipa]] unsigned short
clmul16 (unsigned short x)
{
  return __builtin_clmulg (x, x);
}

[[gnu::noipa]] unsigned short
clmul16w (unsigned short x, unsigned short *y)
{
  return __builtin_clmulg (x, x, y);
}

[[gnu::noipa]] unsigned int
clmul32 (unsigned int x)
{
  return __builtin_clmulg (x, x);
}

[[gnu::noipa]] unsigned int
clmul32w (unsigned int x, unsigned int *y)
{
  return __builtin_clmulg (x, x, y);
}

[[gnu::noipa]] unsigned long long
clmul64 (unsigned long long x)
{
  return __builtin_clmulg (x, x);
}

[[gnu::noipa]] unsigned long long
clmul64w (unsigned long long x, unsigned long long *y)
{
  return __builtin_clmulg (x, x, y);
}

#ifdef __SIZEOF_INT128__
[[gnu::noipa]] unsigned __int128
clmul128 (unsigned __int128 x)
{
  return __builtin_clmulg (x, x);
}

[[gnu::noipa]] unsigned __int128
clmul128w (unsigned __int128 x, unsigned __int128 *y)
{
  return __builtin_clmulg (x, x, y);
}
#endif

int
main ()
{
  unsigned char h8, e8;
  unsigned short h16, e16;
  unsigned int h32, e32;
  unsigned long long h64, e64;
  if (clmul8 (0x64)
      != __builtin_clmulg ((unsigned char) 0x64, (unsigned char) 0x64)
      || (clmul8w (0x64, &h8)
	  != __builtin_clmulg ((unsigned char) 0x64, (unsigned char) 0x64,
			       &e8))
      || h8 != e8
      || (clmul16 (0x477b)
	  != __builtin_clmulg ((unsigned short) 0x477b,
			       (unsigned short) 0x477b))
      || (clmul16w (0x477b, &h16)
	  != __builtin_clmulg ((unsigned short) 0x477b,
			       (unsigned short) 0x477b, &e16))
      || h16 != e16
      || (clmul32 (0xac668e06U)
	  != __builtin_clmulg (0xac668e06U, 0xac668e06U))
      || (clmul32w (0xac668e06U, &h32)
	  != __builtin_clmulg (0xac668e06U, 0xac668e06U, &e32))
      || h32 != e32
      || (clmul64 (0xd5786b341027cb20ULL)
	  != __builtin_clmulg (0xd5786b341027cb20ULL, 0xd5786b341027cb20ULL))
      || (clmul64w (0xd5786b341027cb20ULL, &h64)
	  != __builtin_clmulg (0xd5786b341027cb20ULL, 0xd5786b341027cb20ULL,
			       &e64))
      || h64 != e64)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  unsigned __int128 h128, e128;
  unsigned __int128 v128 = ((((unsigned __int128) 0x6e3e755379242a1fULL) << 64)
			    | 0x49cb5521019109bbULL);
  if (clmul128 (v128) != __builtin_clmulg (v128, v128)
      || clmul128w (v128, &h128) != __builtin_clmulg (v128, v128, &e128)
      || h128 != e128)
    __builtin_abort ();
#endif
}

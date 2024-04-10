/* PR middle-end/113722 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

int
main ()
{
  unsigned __int128 a = __builtin_bswap128 ((unsigned __int128) 2);
  if (a != ((unsigned __int128) 2) << 120)
    __builtin_abort ();
  a = __builtin_bswap128 ((unsigned __int128) 0xdeadbeefULL);
  if (a != ((unsigned __int128) 0xefbeaddeULL) << 96)
    __builtin_abort ();
  a = __builtin_bswap128 (((unsigned __int128) 0xdeadbeefULL) << 64);
  if (a != ((unsigned __int128) 0xefbeaddeULL) << 32)
    __builtin_abort ();
  a = __builtin_bswap128 ((((unsigned __int128) 0xdeadbeefULL) << 64)
			  | 0xcafed00dfeedbac1ULL);
  if (a != ((((unsigned __int128) 0xc1baedfe0dd0fecaULL) << 64)
	    | (((unsigned __int128) 0xefbeaddeULL) << 32)))
    __builtin_abort ();
}

/* { dg-do run { target int128 } } */
/* { dg-options "" } */

struct s
{
  __int128 y : 66;
};
typedef struct s T;
T a[] = { 1, 10000, 0x12345, 0xff000001, 1ULL << 63, (__int128) 1 << 64,
	  ((__int128) 1 << 64) | 1 };

int
main (void)
{
  if (a[0].y != 1
      || a[1].y != 10000
      || a[2].y != 0x12345
      || a[3].y != 0xff000001
      || a[4].y != (1ULL << 63)
      || a[5].y != ((__int128) 1 << 64)
      || a[6].y != (((__int128) 1 << 64) | 1))
    __builtin_abort ();
  return 0;
}

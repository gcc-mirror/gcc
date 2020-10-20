// { dg-do compile { target int128 } }
// { dg-options "-O1 -ftree-vrp" }

__int128
ef (__int128 ms)
{
  int dh = 129;
  int *hj = &dh;

  return ms << *hj ? ms : 0;
}

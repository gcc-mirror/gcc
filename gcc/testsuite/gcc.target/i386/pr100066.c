/* { dg-do compile { target { int128 } } } */
/* { dg-options "-O1 -w" } */
int pm;

void
w3 (int, int, int);

void
e6 (__int128 rt, long int mo)
{
  mo += rt / 0;
  w3 (pm / mo, pm, 0);
}

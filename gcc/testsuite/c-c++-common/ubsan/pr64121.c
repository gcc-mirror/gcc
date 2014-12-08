/* PR sanitizer/64121 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -Wno-pointer-arith" } */

extern int tab[16];

void
execute (int *ip, int x)
{
  int *xp = tab;
base:
  if (x)
    return;
  *xp++ = *ip;
  goto *(&&base + *ip);
}

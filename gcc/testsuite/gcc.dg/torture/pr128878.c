/* { dg-do compile { target bitint575 } } */

struct a { _BitInt(35) x : 6; _BitInt(575) b : 455; };
void c (struct a *);

void d (void)
{
  struct a e[5];
  for (int f = 0; f < 5; f++)
    e[f].b = 0;
  c (e);
}

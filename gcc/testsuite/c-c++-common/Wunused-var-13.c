/* PR c/46015 */
/* { dg-options "-Wunused" } */
/* { dg-do compile } */

int
f1 (int i)
{
  static void *labs[2] = { &&lab1, &&lab2 };
  goto *(labs[i & 1]);

lab1:
  return 1;
lab2:
  return 2;
}

int
f2 (int i)
{
  void *labs[2] = { &&lab1, &&lab2 };
  goto *labs[i & 1];

lab1:
  return 1;
lab2:
  return 2;
}

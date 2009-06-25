/* PR c/32041 */
/* { dg-do compile } */

struct S
{
  int c;
  struct { float f; } sa[2];
};

char a[__builtin_offsetof (struct S, sa->f)
       == __builtin_offsetof (struct S, sa[0].f) ? 1 : -1];


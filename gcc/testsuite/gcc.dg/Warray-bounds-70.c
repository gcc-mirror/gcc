/* PR middle-end/97556 - ICE on excessively large index into a multidimensional
   array
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define SIZE_MAX __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

char a[1][3];

void f (int c)
{
  size_t i = c ? SIZE_MAX / 2 : SIZE_MAX;
  a[i][0] = 0;                          // { dg-warning "\\\[-Warray-bounds" }
}

// { dg-prune-output "\\\[-Wstringop-overflow=" }

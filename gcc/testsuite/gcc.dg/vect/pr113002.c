/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-additional-options "-finline-stringops -Os" } */

typedef __int128 v64u128 __attribute__((vector_size(64)));
int c;
v64u128 u;
void foo() {
  if (c)
    u = (v64u128){0};
  else
    u = (v64u128){1};
}

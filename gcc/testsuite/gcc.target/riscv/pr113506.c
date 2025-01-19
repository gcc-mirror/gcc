/* { dg-do compile } */
/* { dg-options "-fchecking=1 -Os -fno-tree-coalesce-vars -finline-stringops" } */

typedef unsigned v32su __attribute__((vector_size (32)));

v32su foo_v32su_4;

unsigned
foo (v32su v32su_2)
{
  v32su_2 *= v32su_2;
  if (foo_v32su_4[3])
    v32su_2 &= (v32su){};
  return v32su_2[1];
}

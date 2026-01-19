/* { dg-do run } */
/* { dg-additional-options "-fgimple" } */

typedef __INT32_TYPE__ v4si __attribute__((vector_size(16)));
typedef __INT32_TYPE__ v2si __attribute__((vector_size(8)));
typedef char v4qi __attribute__((vector_size(4)));

v4si res;
v2si a;
v2si b;

void __attribute__((noipa)) __GIMPLE() foo ()
{
  v2si a_;
  v2si b_;
  v4si res_;
  a_ = a;
  b_ = b;
  res_ = __VEC_PERM (a_, b_, _Literal (v4qi) { 0, 2, 2, 1 });
  res = res_;
}

int main()
{
  a = (v2si){ 4, 3 };
  b = (v2si){ 2, 1 };
  foo ();
  if (res[0] != 4 || res[1] != 2 || res[2] != 2 || res[3] != 3)
    __builtin_abort ();
  return 0;
}

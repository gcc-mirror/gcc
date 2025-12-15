/* { dg-options "-mlsx -O1" } */

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
#define T BS_VEC(unsigned, 4)

int main ()
{
  BS_VEC(long int, 2) BS_VAR_0[1];
  BS_VEC(int, 4) tt = (BS_VEC(int, 4)){0x9e47d3d2, 0, 0, 0};
  asm("":"+f"(tt));
  BS_VEC(unsigned int, 2) SHUF = __builtin_shufflevector((T)tt, (T)tt, 0, 1);
  BS_VAR_0[0] = __builtin_convertvector(SHUF, BS_VEC(long int, 2));

  if (BS_VAR_0[0][0] != 0x000000009e47d3d2)
    __builtin_abort ();
}

/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mmove-max=256 -mstore-max=256 -fno-stack-protector" } */
/* { dg-final { scan-assembler-times {(?n)vptest.*ymm} 1 } } */
/* { dg-final { scan-assembler-times {sete} 1 } } */
/* { dg-final { scan-assembler-not {(?n)je.*L[0-9]} } } */
/* { dg-final { scan-assembler-not {(?n)jne.*L[0-9]} } } */


_Bool f256(char *a)
{
  char t[] = "0123456789012345678901234567890";
  return __builtin_memcmp(a, &t[0], sizeof(t)) == 0;
}

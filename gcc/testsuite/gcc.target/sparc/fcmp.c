/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef int vec32 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));

long test_fcmple16 (vec16 a, vec16 b)
{
  return __builtin_vis_fcmple16 (a, b);
}

long test_fcmple32 (vec32 a, vec32 b)
{
  return __builtin_vis_fcmple32 (a, b);
}

long test_fcmpne16 (vec16 a, vec16 b)
{
  return __builtin_vis_fcmpne16 (a, b);
}

long test_fcmpne32 (vec32 a, vec32 b)
{
  return __builtin_vis_fcmpne32 (a, b);
}

long test_fcmpgt16 (vec16 a, vec16 b)
{
  return __builtin_vis_fcmpgt16 (a, b);
}

long test_fcmpgt32 (vec32 a, vec32 b)
{
  return __builtin_vis_fcmpgt32 (a, b);
}

long test_fcmpeq16 (vec16 a, vec16 b)
{
  return __builtin_vis_fcmpeq16 (a, b);
}

long test_fcmpeq32 (vec32 a, vec32 b)
{
  return __builtin_vis_fcmpeq32 (a, b);
}

/* { dg-final { scan-assembler "fcmple16\t%" } } */
/* { dg-final { scan-assembler "fcmple32\t%" } } */
/* { dg-final { scan-assembler "fcmpne16\t%" } } */
/* { dg-final { scan-assembler "fcmpne32\t%" } } */
/* { dg-final { scan-assembler "fcmpgt16\t%" } } */
/* { dg-final { scan-assembler "fcmpgt32\t%" } } */
/* { dg-final { scan-assembler "fcmpeq16\t%" } } */
/* { dg-final { scan-assembler "fcmpeq32\t%" } } */

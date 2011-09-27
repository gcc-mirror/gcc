/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */

long test_edge8 (void *p1, void *p2)
{
  return __builtin_vis_edge8 (p1, p2);
}

long test_edge8l (void *p1, void *p2)
{
  return __builtin_vis_edge8l (p1, p2);
}

long test_edge16 (void *p1, void *p2)
{
  return __builtin_vis_edge16 (p1, p2);
}

long test_edge16l (void *p1, void *p2)
{
  return __builtin_vis_edge16l (p1, p2);
}

long test_edge32 (void *p1, void *p2)
{
  return __builtin_vis_edge32 (p1, p2);
}

long test_edge32l (void *p1, void *p2)
{
  return __builtin_vis_edge32l (p1, p2);
}

/* { dg-final { scan-assembler "edge8\t%" } } */
/* { dg-final { scan-assembler "edge8l\t%" } } */
/* { dg-final { scan-assembler "edge16\t%" } } */
/* { dg-final { scan-assembler "edge16l\t%" } } */
/* { dg-final { scan-assembler "edge32\t%" } } */
/* { dg-final { scan-assembler "edge32l\t%" } } */

/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc3 -mvis" } */

long test_edge8n (void *p1, void *p2)
{
  return __builtin_vis_edge8n (p1, p2);
}

long test_edge8ln (void *p1, void *p2)
{
  return __builtin_vis_edge8ln (p1, p2);
}

long test_edge16n (void *p1, void *p2)
{
  return __builtin_vis_edge16n (p1, p2);
}

long test_edge16ln (void *p1, void *p2)
{
  return __builtin_vis_edge16ln (p1, p2);
}

long test_edge32n (void *p1, void *p2)
{
  return __builtin_vis_edge32n (p1, p2);
}

long test_edge32ln (void *p1, void *p2)
{
  return __builtin_vis_edge32ln (p1, p2);
}

/* { dg-final { scan-assembler "edge8n\t%" } } */
/* { dg-final { scan-assembler "edge8ln\t%" } } */
/* { dg-final { scan-assembler "edge16n\t%" } } */
/* { dg-final { scan-assembler "edge16ln\t%" } } */
/* { dg-final { scan-assembler "edge32n\t%" } } */
/* { dg-final { scan-assembler "edge32ln\t%" } } */

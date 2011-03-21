/* { dg-options "-O2" } */
void
f1 (int *p, int x)
{
  asm ("asm1 %0" : "=es" (p[x]));
}

void
f2 (int *p)
{
  while (1)
    {
      p += 4;
      asm ("asm2%U0 %0" : "=m<>" (*p));
    }
}

void
f3 (int *p)
{
  while (1)
    {
      p += 4;
      asm ("asm3%U0 %0" : "=es" (*p));
    }
}

void
f4 (int *p)
{
  asm ("asm4 %0" : "=es" (p[100]));
}

/* { dg-final { scan-assembler "asm1 %?r?3,%?r?4" } } */
/* { dg-final { scan-assembler "asm2u 16\\(%?r?3\\)" } } */
/* { dg-final { scan-assembler "asm3 0\\(%?r?3\\)" } } */
/* { dg-final { scan-assembler "asm4 400\\(%?r?3\\)" } } */

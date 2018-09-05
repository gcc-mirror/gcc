/* { dg-do compile } */
/* { dg-options "-O2 -gsplit-dwarf -g3" } */
/* { dg-additional-options "-march=skylake -mrtm -mabm" { target x86_64-*-* i?86-*-* } } */

int a;
void b(void);

void e(int *);
int f(void);

void 
c(void)
{
  int d;
  e(&d);
  if (f())
    b();
}

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse3" } */
typedef char v16qi __attribute__((vector_size (16)));
typedef float v4sf __attribute__((vector_size (16)));
typedef double v2df __attribute__((vector_size (16)));

extern char ca[];
extern float fa[];
extern double da[];

extern v16qi cva[];
extern v4sf fva[];
extern v2df dva[];

void
foo (void)
{
  cva[0] = __builtin_ia32_loaddqu (ca);
  cva[0] = __builtin_ia32_lddqu (ca);

  fva[0] = __builtin_ia32_loadups (fa);

  dva[0] = __builtin_ia32_loadupd (da);
}

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse3" } */
typedef char v16qi __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));
typedef float v4sf __attribute__((vector_size (16)));
typedef double v2df __attribute__((vector_size (16)));

extern char ca[];
extern int ia[];
extern float fa[];
extern double da[];

extern v16qi cva[];
extern v4si iva[];
extern v4sf fva[];
extern v2df dva[];

void
foo (void)
{
  cva[0] = __builtin_ia32_loaddqa (ca);
  cva[0] = __builtin_ia32_loaddqu (ca);
  cva[0] = __builtin_ia32_lddqu (ca);

  iva[0] = __builtin_ia32_loadd (ia);

  fva[0] = __builtin_ia32_loadaps (fa);
  fva[0] = __builtin_ia32_loadups (fa);
  fva[0] = __builtin_ia32_loadss (fa);

  dva[0] = __builtin_ia32_loadapd (da);
  dva[0] = __builtin_ia32_loadupd (da);
  dva[0] = __builtin_ia32_loadsd (da);
  dva[0] = __builtin_ia32_loadpd1 (da);
  dva[0] = __builtin_ia32_loadrpd (da);
  dva[0] = __builtin_ia32_loadddup (da);
}

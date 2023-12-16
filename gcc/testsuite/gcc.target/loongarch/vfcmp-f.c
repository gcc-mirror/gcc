/* Test mapping IEC 60559 operations to SIMD instructions.
   For details read C23 Annex F.3 and LoongArch Vol. 1 section 3.2.2.1.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -ffixed-f0 -ffixed-f1 -ffixed-f2 -fno-vect-cost-model" } */

#ifndef F
#define F float
#endif

#ifndef I
#define I int
#endif

#ifndef VL
#define VL 16
#endif

typedef F VF __attribute__ ((vector_size (VL)));
typedef I VI __attribute__ ((vector_size (VL)));

register VF a asm ("f0");
register VF b asm ("f1");
register VI c asm ("f2");

void
compare_quiet_equal (void)
{
  c = (a == b);
}

void
compare_quiet_not_equal (void)
{
  c = (a != b);
}

void
compare_signaling_greater (void)
{
  c = (a > b);
}

void
compare_signaling_greater_equal (void)
{
  c = (a >= b);
}

void
compare_signaling_less (void)
{
  c = (a < b);
}

void
compare_signaling_less_equal (void)
{
  c = (a <= b);
}

void
compare_signaling_not_greater (void)
{
  c = ~(a > b);
}

void
compare_signaling_less_unordered (void)
{
  c = ~(a >= b);
}

void
compare_signaling_not_less (void)
{
  c = ~(a < b);
}

void
compare_signaling_greater_unordered (void)
{
  c = ~(a <= b);
}

void
compare_quiet_less (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isless (a[i], b[i]) ? -1 : 0;
}

void
compare_quiet_less_equal (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_islessequal (a[i], b[i]) ? -1 : 0;
}

void
compare_quiet_greater (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isgreater (a[i], b[i]) ? -1 : 0;
}

void
compare_quiet_greater_equal (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isgreaterequal (a[i], b[i]) ? -1 : 0;
}

void
compare_quiet_not_less (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isless (a[i], b[i]) ? 0 : -1;
}

void
compare_quiet_greater_unordered (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_islessequal (a[i], b[i]) ? 0 : -1;
}

void
compare_quiet_not_greater (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isgreater (a[i], b[i]) ? 0 : -1;
}

void
compare_quiet_less_unordered (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isgreaterequal (a[i], b[i]) ? 0 : -1;
}

void
compare_quiet_unordered (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isunordered (a[i], b[i]) ? -1 : 0;
}

void
compare_quiet_ordered (void)
{
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); i++)
    c[i] = __builtin_isunordered (a[i], b[i]) ? 0 : -1;
}

/* The "-<function_name>" matches the .size directive after the function
   body, so we can ensure the instruction is in the correct function.  */

/* { dg-final { scan-assembler "compare_quiet_equal:.*\tvfcmp\\.ceq\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_equal:.*\tvfcmp\\.cune\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_not_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater:.*\tvfcmp\\.slt\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_equal:.*\tvfcmp\\.sle\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less:.*\tvfcmp\\.slt\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_equal:.*\tvfcmp\\.sle\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_greater:.*\tvfcmp\\.sule\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_unordered:.*\tvfcmp\\.sult\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_less:.*\tvfcmp\\.sule\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_not_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_unordered:.*\tvfcmp\\.sult\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less:.*\tvfcmp\\.clt\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_equal:.*\tvfcmp\\.cle\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater:.*\tvfcmp\\.clt\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_equal:.*\tvfcmp\\.cle\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_less:.*\tvfcmp\\.cule\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_not_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_unordered:.*\tvfcmp\\.cult\\.s\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_greater:.*\tvfcmp\\.cule\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_unordered:.*\tvfcmp\\.cult\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_unordered:.*\tvfcmp\\.cun\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_ordered:.*\tvfcmp\\.cor\\.s\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_ordered\n" } } */

/* Test mapping IEC 60559 operations to SIMD instructions.
   For details read C23 Annex F.3 and LoongArch Vol. 1 section 3.2.2.1.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { check-function-bodies "**" "" } } */

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

#define ARGS const VF *a, const VF *b, VI *c

void
compare_quiet_equal (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = (_a == *b);
}

void
compare_quiet_not_equal (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = (_a != *b);
}

void
compare_signaling_greater (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = (_a > *b);
}

void
compare_signaling_greater_equal (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = (_a >= *b);
}

void
compare_signaling_less (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = (_a < *b);
}

void
compare_signaling_less_equal (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = (_a <= *b);
}

void
compare_signaling_not_greater (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = ~(_a > *b);
}

void
compare_signaling_less_unordered (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = ~(_a >= *b);
}

void
compare_signaling_not_less (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = ~(_a < *b);
}

void
compare_signaling_greater_unordered (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  *c = ~(_a <= *b);
}

void
compare_quiet_less (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isless (_a[i], (*b)[i]) ? -1 : 0;
}

void
compare_quiet_less_equal (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_islessequal (_a[i], (*b)[i]) ? -1 : 0;
}

void
compare_quiet_greater (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isgreater (_a[i], (*b)[i]) ? -1 : 0;
}

void
compare_quiet_greater_equal (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isgreaterequal (_a[i], (*b)[i]) ? -1 : 0;
}

void
compare_quiet_not_less (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isless (_a[i], (*b)[i]) ? 0 : -1;
}

void
compare_quiet_greater_unordered (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_islessequal (_a[i], (*b)[i]) ? 0 : -1;
}

void
compare_quiet_not_greater (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isgreater (_a[i], (*b)[i]) ? 0 : -1;
}

void
compare_quiet_less_unordered (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isgreaterequal (_a[i], (*b)[i]) ? 0 : -1;
}

void
compare_quiet_unordered (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isunordered (_a[i], (*b)[i]) ? -1 : 0;
}

void
compare_quiet_ordered (ARGS)
{
  VF _a = *a;
  asm("" ::: "memory");
  for (int i = 0; i < sizeof (*c) / sizeof ((*c)[0]); i++)
    (*c)[i] = __builtin_isunordered (_a[i], (*b)[i]) ? 0 : -1;
}

/*
** compare_quiet_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.ceq.s	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_not_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cune.s	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.slt.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_greater_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sle.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.slt.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_less_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sle.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_not_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sule.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_less_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sult.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_not_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sule.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_signaling_greater_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.sult.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.clt.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_less_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cle.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.clt.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_greater_equal:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cle.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_not_less:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cule.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_greater_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cult.s	(\$vr[0-9]+),\2,\1
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_not_greater:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cule.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_less_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cult.s	(\$vr[0-9]+),\1,\2
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_unordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cun.s	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

/*
** compare_quiet_ordered:
** 	vld	(\$vr[0-9]+),\$r4,0
** 	vld	(\$vr[0-9]+),\$r5,0
** 	vfcmp.cor.s	(\$vr[0-9]+),(\1,\2|\2,\1)
**	vst	\3,\$r6,0
**	jr	\$r1
*/

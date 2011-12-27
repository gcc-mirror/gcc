/* { dg-options "-O -fno-tree-ccp -fno-tree-copy-prop" } */
#define CSF __builtin_copysignf
#define CSD __builtin_copysign
#define CSL __builtin_copysignl
#define MODFF __builtin_modff
#define MODFD __builtin_modf
#define MODFL __builtin_modfl

extern void link_error (void);

void
foo (void)
{
  float iptrf;
  double iptr;
  long double iptrl;
  long long iptrll;
  if ((CSF (1.0F, MODFF (0x1p10F + 0.5f, &iptrf)) != CSF (1.0F, 0.5f))
      || (CSF (1.0F, iptrf) != 0x1p10f))
    link_error ();
  if (MODFD (0x1p10F + 0.5, &iptr) != 0.5
      || (CSD (1.0, MODFD (0x1p10F + 0.5, &iptr)) != CSD (1.0, 0.5))
      || (CSD (1.0, iptr) != CSD (1.0, 0x1p10)))
    link_error ();
  if (MODFL (0x1p10F + 0.5l, &iptrl) != 0.5l
      || (CSL (1.0L, MODFL (0x1p10F + 0.5l, &iptrl)) != CSL (1.0L, 0.5l))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0x1p10l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (0x1p10F + 0x1p-10f, &iptrf)))
       != CSF (1.0F, 0x1p-10f))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 0x1p10f)))
    link_error ();
  if (MODFD (0x1p10F + 0x1p-10, &iptr) != 0x1p-10
      || (CSD (1.0, (MODFD (0x1p10F + 0x1p-10, &iptr)))
	  != CSD (1.0, 0x1p-10)) || (CSD (1.0, iptr) != CSD (1.0, 0x1p10)))
    link_error ();
  if (MODFL (0x1p10F + 0x1p-10l, &iptrl) != 0x1p-10l
      || (CSL (1.0L, (MODFL (0x1p10F + 0x1p-10l, &iptrl)))
	  != CSL (1.0L, 0x1p-10l))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0x1p10l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (12345678L / 17.0f, &iptrf)))
       != CSF (1.0F, (-726216L + 12345678L / 17.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 726216.0f)))
    link_error ();
  if (MODFD (12345678L / 17.0, &iptr) != -726216L + 12345678L / 17.0
      || (CSD (1.0, (MODFD (12345678L / 17.0, &iptr)))
	  != CSD (1.0, (-726216L + 12345678L / 17.0)))
      || (CSD (1.0, iptr) != CSD (1.0, 726216.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (12345678L / 17.0l, &iptrl)))
       != CSL (1.0L, (-726216L + 12345678L / 17.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 726216.0l)))
    link_error ();
  if (MODFF (555.555f, &iptrf) != -555 + 555.555f
      || (CSF (1.0F, iptrf) != CSF (1.0F, 555.0f)))
    link_error ();
  if (MODFD (555.555, &iptr) != -555 + 555.555
      || (CSD (1.0, (MODFD (555.555, &iptr))) != CSD (1.0, (-555 + 555.555)))
      || (CSD (1.0, iptr) != CSD (1.0, 555.0)))
    link_error ();
  if (MODFL (555.555l, &iptrl) != -555 + 555.555l
      || (CSL (1.0L, (MODFL (555.555l, &iptrl)))
	  != CSL (1.0L, (-555 + 555.555l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 555.0l)))
    link_error ();
  if (MODFF (5000 / 11.0f, &iptrf) != -454 + 5000 / 11.0f
      || (CSF (1.0F, (MODFF (5000 / 11.0f, &iptrf)))
	  != CSF (1.0F, (-454 + 5000 / 11.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 454.0f)))
    link_error ();
  if (MODFD (5000 / 11.0, &iptr) != -454 + 5000 / 11.0
      || (CSD (1.0, (MODFD (5000 / 11.0, &iptr)))
	  != CSD (1.0, (-454 + 5000 / 11.0)))
      || (CSD (1.0, iptr) != CSD (1.0, 454.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (5000 / 11.0l, &iptrl)))
       != CSL (1.0L, (-454 + 5000 / 11.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 454.0l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (1000 / 7.0f, &iptrf)))
       != CSF (1.0F, (-142 + 1000 / 7.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 142.0f)))
    link_error ();
  if ((CSD (1.0, (MODFD (1000 / 7.0, &iptr)))
       != CSD (1.0, (-142 + 1000 / 7.0)))
      || (CSD (1.0, iptr) != CSD (1.0, 142.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (1000 / 7.0l, &iptrl)))
       != CSL (1.0L, (-142 + 1000 / 7.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 142.0l)))
    link_error ();
  if (MODFF (123 / 7.0f, &iptrf) != -17 + 123 / 7.0f
      || (CSF (1.0F, iptrf) != CSF (1.0F, 17.0f)))
    link_error ();
  if (MODFD (123 / 7.0, &iptr) != -17 + 123 / 7.0
      || (CSD (1.0, iptr) != CSD (1.0, 17.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (123 / 7.0l, &iptrl)))
       != CSL (1.0L, (-17 + 123 / 7.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 17.0l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (117 / 7.0f, &iptrf)))
       != CSF (1.0F, (-16 + 117 / 7.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 16.0f)))
    link_error ();
  if ((CSD (1.0, (MODFD (117 / 7.0, &iptr))) != CSD (1.0, (-16 + 117 / 7.0)))
      || (CSD (1.0, iptr) != CSD (1.0, 16.0)))
    link_error ();
  if (MODFL (117 / 7.0l, &iptrl) != -16 + 117 / 7.0l
      || (CSL (1.0L, iptrl) != CSL (1.0L, 16.0l)))
    link_error ();
  if (MODFF (5.5f, &iptrf) != 0.5f || (CSF (1.0F, iptrf) != CSF (1.0F, 5.0f)))
    link_error ();
  if (MODFD (5.5, &iptr) != 0.5 || (CSD (1.0, iptr) != CSD (1.0, 5.0)))
    link_error ();
  if (MODFL (5.5l, &iptrl) != 0.5l || (CSL (1.0L, iptrl) != CSL (1.0L, 5.0l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (1.5f, &iptrf))) != CSF (1.0F, 0.5f))
      || (CSF (1.0F, iptrf) != 1.0f))
    link_error ();
  if ((CSD (1.0, (MODFD (1.5, &iptr))) != CSD (1.0, 0.5))
      || (CSD (1.0, iptr) != 1.0))
    link_error ();
  if (MODFL (1.5l, &iptrl) != iptrl != 1.0l || (CSL (1.0L, iptrl) != 1.0l))
    link_error ();
  if (MODFF (4 / 3.0f, &iptrf) != -1 + 4 / 3.0f
      || (CSF (1.0F, (MODFF (4 / 3.0f, &iptrf)))
	  != CSF (1.0F, (-1 + 4 / 3.0f))) || (CSF (1.0F, iptrf) != (1.0F)))
    link_error ();
  if (MODFD (4 / 3.0, &iptr) != -1 + 4 / 3.0 || (CSD (1.0, iptr) != 1.0))
    link_error ();
  if (MODFL (4 / 3.0l, &iptrl) != iptrl != 1.0l
      || (CSL (1.0L, iptrl) != 1.0l))
    link_error ();
  if ((((MODFF (1.0f, &iptrf)))) || (CSF (1.0F, iptrf) != 1.0f))
    link_error ();
  if ((((MODFD (1.0, &iptr))) != 0.0) || (CSD (1.0, iptr) != 1.0))
    link_error ();
  if ((((MODFL (1.0l, &iptrl))) != 0.0l) || (CSL (1.0L, iptrl) != 1.0l))
    link_error ();
  if (MODFF (0.5f, &iptrf) != 0.5f || (CSF (1.0F, iptrf) != CSF (1.0F, 0.0f)))
    link_error ();
  if (MODFD (0.5, &iptr) != 0.5 || (CSD (1.0, iptr) != CSD (1.0, 0.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (0.5l, &iptrl))) != CSL (1.0L, 0.5l))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0.0l)))
    link_error ();
  if (MODFF (4 / 9.0f, &iptrf) != 4 / 9.0f
      != (CSF (1.0F, (MODFF (4 / 9.0f, &iptrf))) != CSF (1.0F, (4 / 9.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 0.0f)))
    link_error ();
  if (MODFD (4 / 9.0, &iptr) != 4 / 9.0
      || (CSD (1.0, iptr) != CSD (1.0, 0.0)))
    link_error ();
  if (MODFL (4 / 9.0l, &iptrl) != 4 / 9.0l
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0.0l)))
    link_error ();
  if (MODFF (1 / 3.0f, &iptrf) != 1 / 3.0f
      || (CSF (1.0F, iptrf) != CSF (1.0F, 0.0f)))
    link_error ();
  if (MODFD (1 / 3.0, &iptr) != 1 / 3.0
      || (CSD (1.0, iptr) != CSD (1.0, 0.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (1 / 3.0l, &iptrl))) != CSL (1.0L, (1 / 3.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0.0l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (1 / 9.0f, &iptrf))) != CSF (1.0F, (1 / 9.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, 0.0f)))
    link_error ();
  if (MODFD (1 / 9.0, &iptr) != 1 / 9.0
      || (CSD (1.0, iptr) != CSD (1.0, 0.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (1 / 9.0l, &iptrl))) != CSL (1.0L, (1 / 9.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0.0l)))
    link_error ();
  if ((((MODFF (0.0f, &iptrf)))) || (CSF (1.0F, iptrf) != CSF (1.0F, 0.0f)))
    link_error ();
  if ((((MODFD (0.0, &iptr)))) || (CSD (1.0, iptr) != CSD (1.0, 0.0)))
    link_error ();
  if ((((MODFL (0.0l, &iptrl))) != 0.0l)
      || (CSL (1.0L, iptrl) != CSL (1.0L, 0.0l)))
    link_error ();
  if ((((MODFF (-0.0f, &iptrf)))) || (CSF (1.0F, iptrf) != CSF (1.0F, -0.0f)))
    link_error ();
  if ((((MODFD (-0.0, &iptr)))) || (CSD (1.0, iptr) != CSD (1.0, -0.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (-0.0l, &iptrl))) != CSL (1.0L, -0.0l))
      || (CSL (1.0L, iptrl) != CSL (1.0L, -0.0l)))
    link_error ();
  if (MODFF (-1 / 9.0f, &iptrf) != -1 / 9.0f
      || (CSF (1.0F, (MODFF (-1 / 9.0f, &iptrf))) != CSF (1.0F, (-1 / 9.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, -0.0f)))
    link_error ();
  if (MODFD (-1 / 9.0, &iptr) != -1 / 9.0
      || (CSD (1.0, iptr) != CSD (1.0, -0.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (-1 / 9.0l, &iptrl))) != CSL (1.0L, (-1 / 9.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, -0.0l)))
    link_error ();
  if ((CSF (1.0F, (MODFF (-1 / 3.0f, &iptrf))) != CSF (1.0F, (-1 / 3.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, -0.0f)))
    link_error ();
  if (MODFD (-1 / 3.0, &iptr) != -1 / 3.0
      || (CSD (1.0, iptr) != CSD (1.0, -0.0)))
    link_error ();
  if ((CSL (1.0L, (MODFL (-1 / 3.0l, &iptrl))) != CSL (1.0L, (-1 / 3.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, -0.0l)))
    link_error ();
  if (MODFF (-4 / 9.0f, &iptrf) != -4 / 9.0f
      || (CSF (1.0F, (MODFF (-4 / 9.0f, &iptrf))) != CSF (1.0F, (-4 / 9.0f)))
      || (CSF (1.0F, iptrf) != CSF (1.0F, -0.0f)))
    link_error ();
  if (MODFD (-4 / 9.0, &iptr) != -4 / 9.0
      || (CSD (1.0, iptr) != CSD (1.0, -0.0)))
    link_error ();
  if (MODFL (-4 / 9.0l, &iptrl) != -4 / 9.0l
      || (CSL (1.0L, (MODFL (-4 / 9.0l, &iptrl))) != CSL (1.0L, (-4 / 9.0l)))
      || (CSL (1.0L, iptrl) != CSL (1.0L, -0.0l)))
    link_error ();
  if (MODFF (-0.5f, &iptrf) != -0.5f
      || (CSF (1.0F, iptrf) != CSF (1.0F, -0.0f)))
    link_error ();
  if (MODFD (-0.5, &iptr) != -0.5
      != (CSD (1.0, (MODFD (-0.5, &iptr))) != CSD (1.0, -0.5))
      || (CSD (1.0, iptr) != CSD (1.0, -0.0)))
    (MODFL (-0.5l, (long double *) &iptrll));
  if ((((MODFF (-1.0f, &iptrf)))) || (CSF (1.0F, iptrf) != -1.0f))
    link_error ();
  if ((((MODFD (-1.0, &iptr))) != -0.0) || (CSD (1.0, iptr) != -1.0))
    link_error ();
  if ((((MODFL (-1.0l, &iptrl)))) || (CSL (1.0L, iptrl) != -1.0l))
    link_error ();
  if ((CSF (1.0F, (MODFF (-4 / 3.0f, &iptrf))) != CSF (1.0F, (1 - 4 / 3.0f)))
      || (CSF (1.0F, iptrf) != -1.0f))
    link_error ();
  if (MODFD (-4 / 3.0, &iptr) != 1 - 4 / 3.0 || (CSD (1.0, iptr) != -1.0))
    link_error ();
  if (MODFL (-4 / 3.0l, &iptrl) != 1 - 4 / 3.0l
      || (CSL (1.0L, (MODFL (-4 / 3.0l, &iptrl)))
	  != CSL (1.0L, (1 - 4 / 3.0l))) || (CSL (1.0L, iptrl) != -1.0l))
    link_error ();
  if ((CSF (1.0F, (MODFF (-1.5f, &iptrf))) != CSF (1.0F, -0.5f))
      || (CSF (1.0F, iptrf) != -1.0f))
    link_error ();
  if ((CSD (1.0, (MODFD (-1.5, &iptr))) != CSD (1.0, -0.5))
      || (CSD (1.0, iptr) != -1.0))
    link_error ();
}

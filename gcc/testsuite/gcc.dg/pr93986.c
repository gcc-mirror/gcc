/* PR tree-optimization/93986 - ICE in decompose, at wide-int.h:984
   { dg-do compile }
   { dg-options "-O1 -foptimize-strlen -ftree-slp-vectorize" }
   { dg-require-effective-target alloca } */

int dd (void);

void ya (int cm)
{
  char s2[cm];

  s2[cm-12] = s2[cm-11] = s2[cm-10] = s2[cm-9]
    = s2[cm-8] = s2[cm-7] = s2[cm-6] = s2[cm-5] = ' ';

  if (dd ())
    __builtin_exit (0);
}

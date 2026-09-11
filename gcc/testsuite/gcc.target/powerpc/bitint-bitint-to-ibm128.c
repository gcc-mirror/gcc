/* { dg-do run { target { bitint && long_double_ibm128 } } } */
/* { dg-options "-std=gnu23 -O2" } */
/* { dg-add-options long_double_ibm128 } */

/* _BitInt -> __ibm128 conversions (__floatbitinttf).  */

double a = 0x1.af497e45ee464p+712;
double b = 0x1.0b477d6d542f5p+658;
double c = 3163434949325304.0;
double e = 0x1.0b454919edbdbp+104;
double f = 0x1.a6c30c79618ap+47;
#if __BITINT_MAXWIDTH__ >= 716
volatile _BitInt(716) ai = 36298058708754201768250943895597234201115462497086093888731075536876907587658549335548553555944875214216312701217238922821070033317711196038381450072091427193009106282776850654141943876956615287227142678490745143296wb;
volatile _BitInt(716) bi = 1248712438979809995633884733030821322590879034671180997836413520081581052515325757236574844006887262637976635409393846961329696177194491997854100272420454130765631968510507532219311958775911742963712wb;
volatile _BitInt(716) ci = 3163434949325304wb;
volatile _BitInt(107) ei = 21175362231015791657227361714176wb;
volatile _BitInt(107) fi = 232415817412805wb;
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 716
  __ibm128 d;
  _BitInt(716) di;
  unsigned _BitInt(716) du;
  di = ai + bi;
  d = di;
  if (d != (__ibm128) a + b)
    __builtin_abort ();
  du = ai + bi;
  d = du;
  if (d != (__ibm128) a + b)
    __builtin_abort ();
  di = ai - bi;
  d = di;
  if (d != (__ibm128) a - b)
    __builtin_abort ();
  du = ai - bi;
  d = du;
  if (d != (__ibm128) a - b)
    __builtin_abort ();
  di = ai + ci;
  d = di;
  if (d != (__ibm128) a + c)
    __builtin_abort ();
  du = ai + ci;
  d = du;
  if (d != (__ibm128) a + c)
    __builtin_abort ();
  di = ai - ci;
  d = di;
  if (d != (__ibm128) a - c)
    __builtin_abort ();
  du = ai - ci;
  d = du;
  if (d != (__ibm128) a - c)
    __builtin_abort ();
  di = -ai + bi;
  d = di;
  if (d != -(__ibm128) a + b)
    __builtin_abort ();
  di = -ai - bi;
  d = di;
  if (d != -(__ibm128) a - b)
    __builtin_abort ();
  di = -ai + ci;
  d = di;
  if (d != -(__ibm128) a + c)
    __builtin_abort ();
  di = -ai - ci;
  d = di;
  if (d != -(__ibm128) a - c)
    __builtin_abort ();
  _BitInt(107) gi;
  unsigned _BitInt(107) gu;
  gi = ei + fi;
  d = gi;
  if (d != (__ibm128) e + f)
    __builtin_abort ();
  gu = ei + fi;
  d = gu;
  if (d != (__ibm128) e + f)
    __builtin_abort ();
  gi = ei - fi;
  d = gi;
  if (d != (__ibm128) e - f)
    __builtin_abort ();
  gu = ei - fi;
  d = gu;
  if (d != (__ibm128) e - f)
    __builtin_abort ();
  gi = -ei + fi;
  d = gi;
  if (d != -(__ibm128) e + f)
    __builtin_abort ();
  gi = -ei - fi;
  d = gi;
  if (d != -(__ibm128) e - f)
    __builtin_abort ();
#endif
}

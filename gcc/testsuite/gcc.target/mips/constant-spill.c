/* { dg-do compile } */
/* { dg-options "-mfp64 -mhard-float -mmsa" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-Os" "-O0" "-O1" "-O2" } { "" } } */

void foo (void);

void bar (void)
{
  int x[4];
  int y[4];
  int i;

  while (1)
    {
      foo ();

      for (i = 0; i < 4; i++)
        {
          x[i] = 0;
          y[i] = 0;
        }

      asm volatile (""
                    :
                    :"m"(x), "m"(y)
                    :"memory");
    }
}

/* { dg-final { scan-assembler-not "ld.w" } } */
/* { dg-final { scan-assembler-times "st.w" 2 } } */

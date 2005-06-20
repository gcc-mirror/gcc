/* This testcase could not assemble on ppc32, because the compiler assumed
   the huge ADDR_DIFF_VEC will be emitted into rodata section, yet because
   of some notes inserted between jump table's CODE_LABEL and the jump table
   it ended up in the .text section and thus shorten_branches couldn't
   figure out branch to lab is too far.  */
/* { dg-do link { target fpic } } */
/* { dg-options "-g1 -fpic" } */
/* { dg-bogus "\[Uu\]nresolved symbol .(_GLOBAL_OFFSET_TABLE_|\[_.A-Za-z\]\[_.0-9A-Za-z\]*@(PLT|GOT|GOTOFF))" "PIC unsupported" { xfail *-*-netware* } 0 } */

#define A(n) \
  case n##1: return n##1 * 131 + 63;	\
  case n##3: return n##3 * 1231 + 182;	\
  case n##5: return n##5 * 351 + 1;	\
  case n##7: return n##7 * 312 + 61;	\
  case n##9: return n##9 * 17 - 1;
#define B(n) \
A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) \
A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
#define C(n) \
B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) \
B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)
#define D(n) \
C(n##0) C(n##1) B(n##20) B(n##21) B(n##22)
      
int
foo (int x)
{
  {
lab:;
    int a = x;
    while (a < 60000)
      {
        int b = a;
        {
          int c = b;
          switch (c)
            {
              D(1)
              default: break;
            }
        }
        a += 10000;
        if (a == 4168)
          goto lab;
      }
  }
  return x;
}

int
main (void)
{
  foo (71);
  return 0;
}

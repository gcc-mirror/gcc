// { dg-do compile { target msp430*-*-* } }
// { dg-options "-std=gnu++11 -fabi-compat-version=0" }
// { dg-skip-if "" { msp430*-*-* } { "-mcpu=msp430" } { "" } }

__int20 x;

__int20 foo (__int20 a, unsigned __int20 b)
{
  return a + b;
}

// { dg-final { scan-assembler "\n_?_Z3foou5int20u6uint20\[: \t\n\]" } }


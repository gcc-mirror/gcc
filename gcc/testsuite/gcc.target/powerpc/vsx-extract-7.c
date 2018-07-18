/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

vector double
test_vpasted (vector double high, vector double low)
{
  vector double res;
  res[1] = high[1];
  res[0] = low[0];
  return res;
}

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1    } } */
/* { dg-final { scan-assembler-not   {\mvspltisw\M}      } } */
/* { dg-final { scan-assembler-not   {\mxxlor\M}         } } */
/* { dg-final { scan-assembler-not   {\mxxlxor\M}        } } */
/* { dg-final { scan-assembler-not   {\mxxspltib\M}      } } */
/* { dg-final { scan-assembler-not   {\mlxvx?\M}         } } */
/* { dg-final { scan-assembler-not   {\mlxv[dw][24]x\M}  } } */
/* { dg-final { scan-assembler-not   {\mlvx\M}           } } */
/* { dg-final { scan-assembler-not   {\mstxvx?\M}        } } */
/* { dg-final { scan-assembler-not   {\mstxv[dw][24]x\M} } } */
/* { dg-final { scan-assembler-not   {\mstvx\M}          } } */

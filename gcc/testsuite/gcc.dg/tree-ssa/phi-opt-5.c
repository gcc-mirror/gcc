/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only -fno-signed-zeros -fdump-tree-phiopt" } */

float repl1 (float varx)
{
  if (varx < 0.0)
    return 0.0;
  else if (varx > 1.0)
    return 1.0;
  else
    return varx;
}

/* Should be turned to

   varx_4 = MIN_EXPR <1.0e+0, varx_2>;
   varx_5 = MAX_EXPR <varx_4, 0.0>;  */  

/* phiopt1 was confused by predictors.  */
/* { dg-final { scan-tree-dump "varx.*MIN_EXPR.*1\\.0" "phiopt1" } } */
/* { dg-final { scan-tree-dump "varx.*MAX_EXPR.*0\\.0" "phiopt1" } } */
/* { dg-final { scan-tree-dump "varx.*MIN_EXPR.*1\\.0" "phiopt2"} } */
/* { dg-final { scan-tree-dump "varx.*MAX_EXPR.*0\\.0" "phiopt2"} } */

float repl2 (float vary)
{
  if (vary > 1.0)
    return 1.0;
  else if (vary < 0.0)
    return 0.0;
  else
    return vary;
}

/* Should be turned to

   vary_4 = MAX_EXPR <0.0, vary_2>;
   vary_5 = MIN_EXPR <vary_4, 1.0e+0>;  */

/* phiopt1 confused by predictors.  */
/* { dg-final { scan-tree-dump "vary.*MAX_EXPR.*0\\.0" "phiopt1" } } */
/* { dg-final { scan-tree-dump "vary.*MIN_EXPR.*1\\.0" "phiopt1" } } */
/* { dg-final { scan-tree-dump "vary.*MAX_EXPR.*0\\.0" "phiopt2"} } */
/* { dg-final { scan-tree-dump "vary.*MIN_EXPR.*1\\.0" "phiopt2"} } */

float repl3 (float varz, float vara, float varb)
{
  if (varz > vara)
    return vara;
  else if (varz < varb)
    return varb;
  else
    return varz;
}

/* Should be turned to

  if (varz_2 > vara_3) goto <L4>; else goto <L1>;

<L1>:;
  vara_6 = MAX_EXPR <varb_5, varz_2>;  */

/* phiopt1 confused by predictors.  */
/* { dg-final { scan-tree-dump "vara.*MAX_EXPR" "phiopt1" } } */
/* { dg-final { scan-tree-dump "if .*varz" "phiopt2"} } */
/* { dg-final { scan-tree-dump "vara.*MAX_EXPR" "phiopt2"} } */

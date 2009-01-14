/* Test for ICE arising from inconsistent use of TARGET_E500 versus
   TARGET_HARD_FLOAT && !TARGET_FPRS.  */
/* { dg-do compile } */
/* { dg-options "-mcpu=750 -mfloat-gprs=single" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } { "*" } { "" } } */

static int comp(const void *a,const void *b){
  return (*(float *)a<*(float *)b)-(*(float *)a>*(float *)b);
}

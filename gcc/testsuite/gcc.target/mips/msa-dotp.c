/* { dg-do compile } */
/* { dg-options "-mno-mips16 -mfp64 -mhard-float -mmsa" } */

typedef int v4i32 __attribute__ ((vector_size(16)));
typedef long long v2i64 __attribute__ ((vector_size(16)));

/* Test MSA dot product family for CSE optimization.  */

static v4i32 g = {0, 92, 93, 94};
static v4i32 h = {12, 24, 36, 48};
static v2i64 l = {84, 98};

void
dotp_d_msa (v2i64 *c)
{
  l = __builtin_msa_dotp_s_d (g, h);
}
/* { dg-final { scan-assembler "dotp_s.d" } }  */

void
dpadd_d_msa (v2i64 *c)
{
  *c = __builtin_msa_dpadd_s_d (l, g, h);
}
/* { dg-final { scan-assembler "dpadd_s.d" } }  */

void
dpsub_d_msa (v2i64 *c)
{
  *c = __builtin_msa_dpsub_s_d (l, g, h);
}
/* { dg-final { scan-assembler "dpsub_s.d" } }  */

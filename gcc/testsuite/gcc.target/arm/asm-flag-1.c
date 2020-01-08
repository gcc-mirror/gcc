/* Test the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-skip-if "" { arm_thumb1 } } */

#ifndef __GCC_ASM_FLAG_OUTPUTS__
#error "missing preprocessor define"
#endif

void f(char *out)
{
  asm(""
      : "=@ccne"(out[0]), "=@cceq"(out[1]),
	"=@cccs"(out[2]), "=@cccc"(out[3]),
	"=@ccmi"(out[4]), "=@ccpl"(out[5]),
	"=@ccvs"(out[6]), "=@ccvc"(out[7]),
	"=@cchi"(out[8]), "=@ccls"(out[9]),
	"=@ccge"(out[10]), "=@cclt"(out[11]),
	"=@ccgt"(out[12]), "=@ccle"(out[13]),
	"=@cchs"(out[14]), "=@cclo"(out[15]));
}

/* There will be at least one of each.  */
/* { dg-final { scan-assembler "movne" } } */
/* { dg-final { scan-assembler "moveq" } } */
/* { dg-final { scan-assembler "movcs" } } */
/* { dg-final { scan-assembler "movcc" } } */
/* { dg-final { scan-assembler "movmi" } } */
/* { dg-final { scan-assembler "movpl" } } */
/* { dg-final { scan-assembler "movvs" } } */
/* { dg-final { scan-assembler "movvc" } } */
/* { dg-final { scan-assembler "movhi" } } */
/* { dg-final { scan-assembler "movls" } } */
/* { dg-final { scan-assembler "movge" } } */
/* { dg-final { scan-assembler "movls" } } */
/* { dg-final { scan-assembler "movgt" } } */
/* { dg-final { scan-assembler "movle" } } */

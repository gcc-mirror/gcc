/* Test the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

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

/* { dg-final { scan-assembler "cset.*, ne" } } */
/* { dg-final { scan-assembler "cset.*, eq" } } */
/* { dg-final { scan-assembler "cset.*, cs" } } */
/* { dg-final { scan-assembler "cset.*, cc" } } */
/* { dg-final { scan-assembler "cset.*, mi" } } */
/* { dg-final { scan-assembler "cset.*, pl" } } */
/* { dg-final { scan-assembler "cset.*, vs" } } */
/* { dg-final { scan-assembler "cset.*, vc" } } */
/* { dg-final { scan-assembler "cset.*, hi" } } */
/* { dg-final { scan-assembler "cset.*, ls" } } */
/* { dg-final { scan-assembler "cset.*, ge" } } */
/* { dg-final { scan-assembler "cset.*, ls" } } */
/* { dg-final { scan-assembler "cset.*, gt" } } */
/* { dg-final { scan-assembler "cset.*, le" } } */

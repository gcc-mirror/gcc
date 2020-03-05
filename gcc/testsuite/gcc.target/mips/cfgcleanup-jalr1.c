/* { dg-do compile } */
/* { dg-options "-mabicalls -fpic -mno-mips16 -mno-micromips" } */
/* { dg-skip-if "needs codesize optimization" { *-*-* } { "-O0" "-O1" "-O2" "-O3" } { "" } } */

extern void foo (void*);

extern void bar (void*);

void
test (void* p)
{
   if (!p)
	foo(p);
   else
	bar(p);
}

/* { dg-final { scan-assembler-not "\\\.reloc\t1f,R_MIPS_JALR,foo" } } */
/* { dg-final { scan-assembler-not "\\\.reloc\t1f,R_MIPS_JALR,bar" } } */

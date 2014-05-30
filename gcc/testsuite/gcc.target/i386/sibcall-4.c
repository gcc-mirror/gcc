/* Testcase for PR target/46219.  */
/* { dg-do compile { xfail { *-*-* } } } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2" } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch[256];

void male_indirect_jump (long offset)
{
  dispatch[offset](offset);
}

/* { dg-final { scan-assembler-not "jmp[ \t]*.%eax" } } */

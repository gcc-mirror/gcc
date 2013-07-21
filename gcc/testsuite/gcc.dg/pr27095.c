/* { dg-do compile } */
/* { dg-options "-O2" } */

/* On MIPS, disable generating hints (R_MIPS_JALR) for PIC calls.  In addition
   to the load from the GOT this also contains the name of the function so for
   each call the function name would appear twice.  */
/* { dg-options "-O2 -mno-relax-pic-calls" { target mips*-*-* } } */

extern void *memset (void *, int, __SIZE_TYPE__);
extern __SIZE_TYPE__ strlen (const char *);

int
main (int argc, char **argv)
{
  char x[8] = "abc";
  memset (x, argc, strlen (x));
  return 0;
}
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen" { target { ! { powerpc*-*-darwin* hppa*-*-hpux* ia64-*-hpux* alpha*-*-* spu-*-* tic6x-*-* } } } } } */
/* hppa*-*-hpux* has an IMPORT statement for strlen (plus the branch). */
/* *-*-darwin* has something similar. */
/* tic6x emits a comment at the point where the delayed branch happens.  */
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen\(.*\n\)+.*strlen" { target hppa*-*-hpux* tic6x-*-* } } } */
/* { dg-final { scan-assembler-not "(?n)bl L_strlen\(.*\n\)+.*bl L_strlen" { target powerpc*-*-darwin* } } } */
/* ia64-*-hpux* has a global statement, a type statement, and the branch. */
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen\(.*\n\)+.*strlen\(.*\n\)+.*strlen" { target ia64-*-hpux* } } } */
/* alpha-*-* has a GOT load and the call.  */
/* { dg-final { scan-assembler-not "(?n)jsr .*,strlen\(.*\n\)+.*jsr .*,strlen" { target alpha*-*-* } } } */
/* spu-*-* has a branch hint and the call.  */
/* { dg-final { scan-assembler-not "(?n)brsl.*,strlen\(.*\n\)+.*brsl.*,strlen" { target spu-*-* } } } */

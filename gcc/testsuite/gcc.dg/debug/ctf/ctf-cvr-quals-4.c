/* Test compilation of stubs with various qualifiers - const, restrict and
   volatile.

   CTF records for CVR qualifiers are no-name records.  In this testcase, there
   is 1 const qualifier. 1 more no-name CTF record corresponds to the CTF
   pointer record.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*ctt_name" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

typedef const struct howto_struct howto_type;

typedef struct entry
{
  int addend;
  howto_type *howto;
} how_ent;

how_ent hent;

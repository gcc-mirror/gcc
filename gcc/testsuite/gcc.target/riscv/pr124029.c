/* { dg-do compile */
/* { dg-options "-march=rv64gcvb_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gcvb_zicond -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

void frob (void);
void
refine_subpel (int cost, int b_chroma_me)
{

  int bcost = bcost = (1 << 28);
  if (b_chroma_me && cost < bcost)
    frob ();
}

/* { dg-final { scan-assembler-not "addi\t" } } */
/* { dg-final { scan-assembler-not "seqz\t" } } */


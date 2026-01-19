/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" { target rv64} } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" { target rv32} } */

unsigned char a;

int main() {
  short b = a = 0;
  for (; a != 19; a++)
    if (a)
      b = 32872 >> a;

  if (b == 0)
    return 0;
  else
    return 1;
}

/* If we vectorized, we should still be able to collapse away the VEC_EXTRACT,
   leaving zero vector code in the final assembly.  So there should be no 
   vsetvl instructions.  */
/* { dg-final { scan-assembler-not {vsetivli} } } */



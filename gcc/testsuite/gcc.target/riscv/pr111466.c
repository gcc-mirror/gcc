/* Simplified varaint of gcc.target/riscv/zba-adduw.c.  */

/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned int foo2(int unused, int n, unsigned y, unsigned delta){
  int s = 0;
  unsigned int x = 0;
  for (;x<n;x +=delta)
    s += x+y;
  return s;
}

/* { dg-final { scan-assembler-not "\msext\M" } } */

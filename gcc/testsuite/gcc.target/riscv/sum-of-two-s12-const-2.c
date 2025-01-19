/* Reduced from glibc/stdio-common/tempnam.c.
   Can't have invalid insn in final output:
      add s0, sp, 2048    */

/* { dg-do compile } */
/* { dg-options { -march=rv64gcv -mabi=lp64d -O2 } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "O1" "-Og" "-Os" "-Oz" } } */

int a(char x[]) {
  char b[4096];
  if (a(b))
    a(b);
}

/* { dg-final { scan-assembler-not {add\t[a-x0-9]+,sp,2048} } } */

/* Test that LoongArch backend stack drop operation optimized.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -fno-stack-protector" } */
/* { dg-final { scan-assembler "addi.d\t\\\$r3,\\\$r3,-16" } } */

extern int printf (char *, ...);

int main()
{
  char buf[1024 * 12];
  printf ("%p\n", buf);
  return 0;
}


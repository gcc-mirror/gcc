// TBD: This doesn't quite work for rv32 yet
/* { dg-do compile } */
/* { dg-options { -march=rv64gcv -mabi=lp64d } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

/* Ensure that gcc doesn't generate standlone li reg, 4096.  */
long
plus1(unsigned long i)
{
   return i + 2048;
}

long
plus2(unsigned long i)
{
   return i + 4094;
}

long
plus3(unsigned long i)
{
   return i + 2064;
}

/* Ensure that gcc doesn't generate standlone li reg, -4096.  */
long
minus1(unsigned long i)
{
   return i - 4096;
}

long
minus2(unsigned long i)
{
   return i - 2049;
}

long
minus3(unsigned long i)
{
   return i - 2064;
}

/* { dg-final { scan-assembler-not {li\t[a-x0-9]+,-4096} } } */
/* { dg-final { scan-assembler-not {li\t[a-x0-9]+,4096} } } */

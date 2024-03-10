/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32d" { target { rv32 } } } */

#define SOME_NUMBER 0x1000

unsigned long
d (unsigned long n)
{
  return n > SOME_NUMBER ? SOME_NUMBER : n;
}

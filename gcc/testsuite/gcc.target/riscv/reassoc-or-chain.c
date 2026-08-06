/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-options "-O3 -march=rv64gc -mabi=lp64d -mtune=generic-ooo -fdump-tree-reassoc2-details" } */

unsigned long
or4 (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return a | b | c | d;
}

/* { dg-final { scan-tree-dump "Width = 2 was chosen" "reassoc2" } } */

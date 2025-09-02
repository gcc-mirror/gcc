/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zilsd -mabi=ilp32d" } */
int foo()
{
}
/* { dg-error "'-march=rv64g.*zilsd.*': zilsd extension supports in rv32 only" "" { target *-*-* } 0 } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zclsd -mabi=ilp32d" } */
int foo()
{
}
/* { dg-error "'-march=rv64.*zclsd.*': zilsd extension supports in rv32 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv64.*zclsd.*': zclsd extension supports in rv32 only" "" { target *-*-* } 0 } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zclsd -mabi=ilp32d" } */
int foo()
{
}
/* { dg-error "'-march=rv64gc_zclsd': zilsd extension supports in rv32 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv64gc_zclsd': zclsd extension supports in rv32 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv64imafdc_zicsr_zifencei_zilsd_zmmul_zaamo_zalrsc_zca_zcd_zclsd': zilsd extension supports in rv32 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv64imafdc_zicsr_zifencei_zilsd_zmmul_zaamo_zalrsc_zca_zcd_zclsd': zclsd extension supports in rv32 only" "" { target *-*-* } 0 } */

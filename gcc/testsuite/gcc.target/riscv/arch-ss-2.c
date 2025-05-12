/* { dg-do compile } */
/* { dg-options "-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm -mabi=ilp32d" } */
int foo()
{
}
/* { dg-error "'-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm': ssnpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm': smnpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm': smmpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm': sspm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm': supm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32imafdc_zicsr_zifencei_zmmul_zaamo_zalrsc_zca_zcd_zcf_smmpm_smnpm_ssnpm_sspm_supm': ssnpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32imafdc_zicsr_zifencei_zmmul_zaamo_zalrsc_zca_zcd_zcf_smmpm_smnpm_ssnpm_sspm_supm': smnpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32imafdc_zicsr_zifencei_zmmul_zaamo_zalrsc_zca_zcd_zcf_smmpm_smnpm_ssnpm_sspm_supm': smmpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32imafdc_zicsr_zifencei_zmmul_zaamo_zalrsc_zca_zcd_zcf_smmpm_smnpm_ssnpm_sspm_supm': sspm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32imafdc_zicsr_zifencei_zmmul_zaamo_zalrsc_zca_zcd_zcf_smmpm_smnpm_ssnpm_sspm_supm': supm extension supports in rv64 only" "" { target *-*-* } 0 } */

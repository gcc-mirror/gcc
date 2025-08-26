/* { dg-do compile } */
/* { dg-options "-march=rv32gc_ssnpm_smnpm_smmpm_sspm_supm -mabi=ilp32d" } */
int foo()
{
}
/* { dg-error "'-march=rv32.\*ssnpm.*': ssnpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32.\*smnpm.*': smnpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32.\*smmpm.*': smmpm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32.\*sspm.*': sspm extension supports in rv64 only" "" { target *-*-* } 0 } */
/* { dg-error "'-march=rv32.\*supm.*': supm extension supports in rv64 only" "" { target *-*-* } 0 } */

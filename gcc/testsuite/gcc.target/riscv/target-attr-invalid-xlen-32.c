/* { dg-do compile { target rv32 } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d" } */

__attribute__ ((target ("cpu=sifive-e31;arch=rv64i")))
void invalid_arch (void) {} /* { dg-error "must start with rv32" } */

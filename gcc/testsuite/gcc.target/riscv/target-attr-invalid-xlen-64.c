/* { dg-do compile { target rv64 } } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */

__attribute__ ((target ("cpu=sifive-u74;arch=rv32i")))
void invalid_arch (void) {} /* { dg-error "must start with rv64" } */

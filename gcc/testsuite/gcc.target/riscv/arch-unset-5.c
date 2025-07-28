/* { dg-do compile } */
/* { dg-options "-march=rv64i -march=unset -mabi=lp64 -misa-spec=20191213" } */
int foo()
{
}

/* { dg-error "At least one valid -mcpu option must be given after -march=unset" "" { target { "riscv*-*-*" } } 0 } */

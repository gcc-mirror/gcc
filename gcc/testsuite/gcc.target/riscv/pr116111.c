/* { dg-do compile } */
/* { dg-options "-march=rv32ed -mabi=ilp32e" } */
int
foo ()
{}

/* { dg-error "ILP32E ABI does not support the 'D' extension" "" { target *-*-* } 0 } */

/* { dg-do compile } */
/* { dg-options "-march=rv64ge -mabi=lp64d" } */
int foo()
{
}

/* { dg-error "ISA string is not in canonical order. 'e'" "" { target *-*-* } 0 } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gzb -mabi=lp64" } */
int foo()
{
}

/* { dg-error "extension 'zb' starts with 'z' but is unsupported standard extension" "" { target *-*-* } 0 } */

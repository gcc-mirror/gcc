/* { dg-do compile } */
/* { dg-options "-march=rv64i -mabi=lp64d" } */
int
foo ()
{}

/* { dg-error "requested ABI requires '-march' to subsume the 'D' extension" "" { target *-*-* } 0 } */

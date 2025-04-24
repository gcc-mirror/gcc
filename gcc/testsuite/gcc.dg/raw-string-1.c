/* PR c++/110343 */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

const void *s0 = R"0123456789abcdefg()0123456789abcdefg" 0;
	/* { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 } */
	/* { dg-error "stray" "stray" { target *-*-* } .-2 } */
const void *s1 = R" () " 0;
	/* { dg-error "invalid character" "invalid" { target *-*-* } .-1 } */
	/* { dg-error "stray" "stray" { target *-*-* } .-2 } */
const void *s2 = R"	()	" 0;
	/* { dg-error "invalid character" "invalid" { target *-*-* } .-1 } */
	/* { dg-error "stray" "stray" { target *-*-* } .-2 } */
const void *s3 = R")())" 0;
	/* { dg-error "invalid character" "invalid" { target *-*-* } .-1 } */
	/* { dg-error "stray" "stray" { target *-*-* } .-2 } */
const char *s4 = R"@()@";
const char *s5 = R"$()$";
const char *s6 = R"`()`";
const void *s7 = R"\u0040()\u0040" 0;
	/* { dg-error "invalid character" "invalid" { target *-*-* } .-1 } */
	/* { dg-error "stray" "stray" { target *-*-* } .-2 } */
const char *s8 = R"`@$$@`@`$()`@$$@`@`$";

int main () {}

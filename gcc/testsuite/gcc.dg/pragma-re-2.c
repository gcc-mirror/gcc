
#pragma redefine_extname		/* { dg-warning "malformed" } */
#pragma redefine_extname foo		/* { dg-warning "malformed" } */
#pragma redefine_extname foo 1		/* { dg-warning "malformed" } */
#pragma redefine_extname foo bar 2	/* { dg-warning "junk" } */

int bar; /* silence `ISO C forbids an empty translation unit' warning */

/* { dg-do compile { target *-*-solaris* } } */

#pragma redefine_extname		/* { dg-warning "malformed" } */
#pragma redefine_extname foo		/* { dg-warning "malformed" } */
#pragma redefine_extname foo 1		/* { dg-warning "malformed" } */
#pragma redefine_extname foo bar 2	/* { dg-warning "junk" } */

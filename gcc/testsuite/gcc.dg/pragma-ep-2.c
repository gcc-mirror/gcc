/* { dg-do compile { target *-*-osf5* } } */

#pragma extern_prefix			/* { dg-warning "malformed" } */
#pragma extern_prefix foo		/* { dg-warning "malformed" } */
#pragma extern_prefix "foo" 1		/* { dg-warning "junk" } */

int bar; /* silence `ISO C forbids an empty translation unit' warning */

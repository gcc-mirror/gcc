/* { dg-do compile } */
/* { dg-options "-fsanitize=address -fsanitize=thread -w" } */

int i;

/* { dg-error ".-fsanitize=thread. is incompatible with .-fsanitize=address|kernel-address." "" { target *-*-* } 0 } */

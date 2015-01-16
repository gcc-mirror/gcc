/* Test -f*sanitize*=all */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=all" } */

int i;

/* { dg-error "-fsanitize=all option is not valid" "" { target *-*-* } 0 } */

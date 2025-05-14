/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

char a1[] = "a";
char a2[1] = "a";	/* { dg-warning "initializer-string for array of 'char' is too long for C\\\+\\\+" } */
char a3[2] = "a";

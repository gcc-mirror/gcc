/* { dg-do compile } */
/* { dg-options "-Wunused-parameter" } */

static int a = 10; /* { dg-warning "defined but not used" } */


/* { dg-do compile } */
/* { dg-options "-Wunused" } */

static int a = 10; /* { dg-warning "defined but not used" } */


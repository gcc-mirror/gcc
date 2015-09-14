/* { dg-do compile } */
/* { dg-options "-Wunused-variable" } */

static int a = 0;	  /* { dg-warning "defined but not used" } */
static const int b = 0;	  /* { dg-warning "defined but not used" } */
static int c __attribute__ ((unused)) = 0;
static const char rcsid[] __attribute__ ((unused)) = "version-string";

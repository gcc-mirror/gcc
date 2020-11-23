/* { dg-do compile } */
/* { dg-require-effective-target noinit } */
/* { dg-options "-Wattributes" } */

/* Check warning/error messages for "noinit" attribute misuse.  */
int __attribute__((noinit)) noinit_fn (void); /* { dg-warning "ignoring 'noinit' attribute not set on a variable" } */
int __attribute__((section ("mysection"), noinit)) noinit_section1; /* { dg-warning "because it conflicts with attribute" } */
int __attribute__((noinit, section ("mysection"))) noinit_section2; /* { dg-warning "because it conflicts with attribute" } */
const int __attribute__((noinit)) noinit_const; /* { dg-warning "ignoring 'noinit' attribute set on const variable" } */
/* { dg-error "uninitialized 'const noinit_const'" "" { target c++ } .-1 } */
int __attribute__((noinit)) noinit_init = 42; /* { dg-warning "ignoring 'noinit' attribute set on initialized variable" } */
void foo (void) { int __attribute__((noinit)) local_noinit; } /* { dg-error "'noinit' attribute cannot be specified for local variables" } */

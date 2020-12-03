/* { dg-do compile } */
/* { dg-require-effective-target persistent } */
/* { dg-options "-Wattributes" } */

/* Check warning/error messages for "persistent" attribute misuse.  */
int __attribute__((persistent)) persistent_fn (void); /* { dg-warning "ignoring 'persistent' attribute not set on a variable" } */
int __attribute__((section ("mysection"), persistent)) persistent_section1 = 1; /* { dg-warning "because it conflicts with attribute" } */
int __attribute__((persistent, section ("mysection"))) persistent_section2 = 2; /* { dg-warning "because it conflicts with attribute" } */
const int __attribute__((persistent)) persistent_const = 3; /* { dg-warning "ignoring 'persistent' attribute set on const variable" } */
int __attribute__((persistent)) persistent_init; /* { dg-warning "ignoring 'persistent' attribute set on uninitialized variable" } */
void foo (void) { int __attribute__((persistent)) local_persistent = 4; } /* { dg-error "'persistent' attribute cannot be specified for local variables" } */

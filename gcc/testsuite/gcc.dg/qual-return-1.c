/* Test for warnings for qualified function return types.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wreturn-type -Wignored-qualifiers" } */

/* Qualifying a function return type makes no sense.  */

const int int_fn (void); /* { dg-warning "qualifiers" "int decl" } */
const int (*int_ptr) (void); /* { dg-warning "qualifiers" "int ptr" } */
const int int_fn2 (void) { return 0; } /* { dg-warning "qualifiers" "int defn" } */

const void void_fn (void); /* { dg-warning "qualifiers" "void decl" } */
const void (*void_ptr) (void); /* { dg-warning "qualifiers" "void ptr" } */
const void void_fn2 (void) { } /* { dg-warning "qualified" "void defn" } */

volatile void vvoid_fn (void); /* { dg-warning "qualifiers" "void decl" } */
volatile void (*vvoid_ptr) (void); /* { dg-warning "qualifiers" "void ptr" } */
volatile void vvoid_fn2 (void) { } /* { dg-warning "qualified" "void defn" } */

int *restrict ip_fn (void); /* { dg-warning "qualifiers" "restrict decl" } */
int *restrict (*ip_ptr) (void); /* { dg-warning "qualifiers" "restrict ptr" } */
int *restrict ip_fn2 (void) { return (int *)0; }; /* { dg-warning "qualifiers" "restrict defn" } */

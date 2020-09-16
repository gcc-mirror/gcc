/* PR c/96126 - conflicting attribute section accepted on redeclaration
   { dg-do compile }
   { dg-options "-Wall" }
   { dg-require-named-sections "" } */

__attribute__ ((section ("s1"))) void f1 (void);
__attribute__ ((section ("s2"))) void f1 (void);  // { dg-warning "ignoring attribute 'section \\\(\"s2\"\\\)' because it conflicts with previous 'section \\\(\"s1\"\\\)'" }

__attribute__ ((section ("s3"), section ("s4")))
void f2 (void);                                   // { dg-error "conflicts" }

__attribute__ ((section ("s5"))) __attribute ((section ("s6")))
void f3 (void);                                   // { dg-error "conflicts" }

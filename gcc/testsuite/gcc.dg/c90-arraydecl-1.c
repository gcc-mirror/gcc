/* Test for C99 forms of array declarator: rejected in C90.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

/* Use of [*] (possibly with type qualifiers) in an array declarator with
   function prototype scope is a C99 feature.  GCC does not yet implement
   it correctly, so gives a warning about this. so we can't yet test here
   that we get just one error and no warnings.  */

void foo0 (int a, int b[*]); /* { dg-error "ISO C89" "\[*\] not in C89" } */
/* { dg-warning "implement" "\[*\] not implemented" { target *-*-* } 11 } */
void foo1 (int, int [*]); /* { dg-error "ISO C89" "\[*\] not in C89" } */
/* { dg-warning "implement" "\[*\] not implemented" { target *-*-* } 13 } */

/* Use of static and type qualifiers (not allowed with abstract declarators)
   is a C99 feature.  */

void bar0 (int a[const]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "ISO C89" "\[quals\] not in C89" { target *-*-* } 19 } */
void bar1 (int a[const 2]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "ISO C89" "\[quals expr\] not in C89" { target *-*-* } 21 } */
void bar2 (int a[static 2]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "ISO C89" "\[static expr\] not in C89" { target *-*-* } 23 } */
void bar3 (int a[static const 2]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "ISO C89" "\[static quals expr\] not in C89" { target *-*-* } 25 } */
void bar4 (int a[const static 2]); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "ISO C89" "\[quals static expr\] not in C89" { target *-*-* } 27 } */

/* Because [*] isn't properly implemented and so warns, we don't test here
   for [const *] yet.  */

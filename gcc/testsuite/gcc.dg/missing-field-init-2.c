/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers -std=c99" } */

struct s { int a, b, c; };
struct s s1 = { 1, 2, 3 };
struct s s2 = { 1, 2 }; /* { dg-warning "(missing initializer)|(near initialization)" } */
struct s s3[] = { { 1, 2 }, { 4, 5 } }; /* { dg-warning "(missing initializer)|(near initialization)" } */
struct s s4[] = { 1, 2, 3, 4, 5 }; /* { dg-warning "(missing initializer)|(near initialization)" } */
struct s s5[] = { 1, 2, 3, 4, 5, 6 };
/* Designated initializers produce no warning.  */
struct s s6 = { .a = 1 }; /* { dg-bogus "missing initializer" } */
/* Allow zero-initializing with "= { 0 }".  */
struct s s7 = { 0 }; /* { dg-bogus "missing initializer" } */
struct s s8 = { 1 }; /* { dg-warning "(missing initializer)|(near initialization)" } */

/* { dg-do compile } */
/* { dg-options "-Wextra -Wno-missing-field-initializers" } */

struct s { int a, b, c; };
struct s s1 = { 1, 2, 3 };
struct s s2 = { 1, 2 }; /* { dg-bogus "missing initializer" } */
struct s s3[] = { { 1, 2 }, { 4, 5 } }; /* { dg-bogus "missing initializer" } */
struct s s4[] = { 1, 2, 3, 4, 5 }; /* { dg-bogus "missing initializer" } */
struct s s5[] = { 1, 2, 3, 4, 5, 6 };

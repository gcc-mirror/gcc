/* Test C23 constexpr.  Invalid types of integer initializers (bug 115515).  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

struct s { float x; };
const struct s i = { 3.1 };
constexpr int j = i.x; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */

constexpr struct s i2 = { 3.25f };
constexpr int j2 = i2.x; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */

constexpr int j3 = 2 * 2.5; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */
constexpr int j4 = 5.0; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */

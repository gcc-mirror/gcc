/* PR c/64417 */
/* { dg-do compile } */
/* { dg-options "" } */

struct foo { int x; char y[]; };
struct bar { struct foo f; };
struct baz { struct bar b; };

struct foo a1 = { 1, "abc" };
struct foo a2 = { 1, { "abc" } };
struct foo b1[] = { { 1, "abc" } }; /* { dg-error "initialization of flexible array member" } */
struct foo b2[] = { { 1, { "abc" } } }; /* { dg-error "initialization of flexible array member" } */
struct bar c1[] = { { { 1, "abc" } } }; /* { dg-error "initialization of flexible array member" } */
struct bar c2[] = { { { 1, { "abc" } } } }; /* { dg-error "initialization of flexible array member" } */
struct baz d1[] = { { { { 1, "abc" } } } }; /* { dg-error "initialization of flexible array member" } */
struct baz d2[] = { { { { 1, { "abc" } } } } }; /* { dg-error "initialization of flexible array member" } */

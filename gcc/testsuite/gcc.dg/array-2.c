/* { dg-do compile } */
/* { dg-options "" } */

/* Verify that we can't do things to get ourselves in trouble
   with GCC's zero-length array extension.  */

struct f { int w; int x[0]; };
struct g { struct f f; };
struct g g1 = { { 0, { } } };
struct g g2 = { { 0, { 1 } } }; /* { dg-error "(nested structure)|(near initialization)" "nested" } */

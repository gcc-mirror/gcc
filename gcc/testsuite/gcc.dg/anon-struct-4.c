/* Test anonymous structures with type qualifiers.  Bug 8420.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

struct s {
  struct { int a; };
  const struct { int b; };
  struct { int c; } volatile;
};

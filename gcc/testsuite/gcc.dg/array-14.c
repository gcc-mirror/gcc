/* { dg-do compile } */
/* { dg-options "" } */

/* Verify that GCC forbids non-static initialization of
   flexible array members. */

typedef char T[];
struct str { int len; T s; };

struct str a = { 2, "a" };

void foo()
{
  static struct str b = { 2, "b" };
  struct str c = { 2, "c" }; /* { dg-error "(non-static)|(near initialization)" } */
  struct str d = (struct str) { 2, "d" }; /* { dg-error "(non-static)|(near initialization)" } */
  struct str e = (struct str) { d.len, "e" }; /* { dg-error "(non-static)|(initialization)" } */
}

/* PR c/5597 */
/* { dg-do compile } */
/* { dg-options "" } */

/* Verify that GCC forbids non-static initialization of
   flexible array members. */

struct str { int len; char s[]; };

struct str a = { 2, "a" };

void foo()
{
  static struct str b = { 2, "b" };
  struct str c = { 2, "c" }; /* { dg-error "(non-static)|(near initialization)" } */
  struct str d = (struct str) { 2, "d" }; /* { dg-error "(non-static)|(near initialization)" } */
  struct str e = (struct str) { d.len, "e" }; /* { dg-error "(non-static)|(initialization)" } */
}

struct str f = { 0, {} };

void bar()
{
  static struct str g = { 0, {} };
  struct str h = { 0, {} }; /* { dg-error "(non-static)|(near initialization)" } */
  struct str i = (struct str) { 0, {} }; /* { dg-error "(non-static)|(near initialization)" } */
  struct str j = (struct str) { i.len, {} }; /* { dg-error "(non-static)|(initialization)" } */
}

struct str k = { 0 };

void baz()
{
  static struct str l = { 0 };
  struct str m = { 0 };
  struct str n = (struct str) { 0 };
  struct str o = (struct str) { n.len };
}

struct str p = {};

void qux()
{
  static struct str q = {};
  struct str r = {};
  struct str s = (struct str) {};
}

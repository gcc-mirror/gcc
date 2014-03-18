// PR sanitizer/59250
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

struct E {
 int i;
};

struct S {
  const char *s;
  S (const char *);
  static E *e;
};

S::S (const char *) : s (0)
{
  e = new E ();
}

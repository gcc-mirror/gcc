// PR c++/64629
// { dg-options "-Wformat -Wformat-security" }

extern void bar (int, const char *, ...) __attribute__((format (printf, 2, 3)));
void
foo (void)
{
  const char *const msg = "abc";
  bar (1, msg);
}

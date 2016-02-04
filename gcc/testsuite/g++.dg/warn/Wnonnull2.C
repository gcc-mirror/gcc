// PR c++/68767
// { dg-options "-Wnonnull" }

extern int len (const char*) __attribute__ ((__nonnull__ (1)));

int f (int x)
{
  return len ((x ? "x" : 0) ? (x ? "x" : 0) : "x");
}

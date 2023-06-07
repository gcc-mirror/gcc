// PR c++/107310

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

struct f
{
  ~f();
};

int foo(int t) {
  f g;
  switch (t) {
  case 1:
    return 1;
  }
  if (true)
    throw 1;
} // { dg-bogus "control reaches end" }

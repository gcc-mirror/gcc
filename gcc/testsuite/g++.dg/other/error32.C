// PR c++/33492
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "" }

void foo()
{
  if (throw 0) // { dg-error "could not convert .\\<throw-expression\\>. from .void. to .bool." }
    ;
}

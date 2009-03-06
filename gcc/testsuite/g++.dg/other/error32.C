// PR c++/33492
// { dg-options "" }

void foo()
{
  if (throw 0) // { dg-error "could not convert .\\<throw-expression\\>. to .bool." }
    ;
}

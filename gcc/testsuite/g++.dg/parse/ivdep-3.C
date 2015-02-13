// PR c++/60211

void foo()
{}
#pragma GCC ivdep  // { dg-error "must be inside a function" }
  for (int i = 0; i < 2; ++i)  // { dg-error "expected|type" }
    ;
}  // { dg-error "expected" }

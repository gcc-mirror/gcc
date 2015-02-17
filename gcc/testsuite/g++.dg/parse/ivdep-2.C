// PR c++/60211

void foo()
{}
  int i;
#pragma GCC ivdep  // { dg-error "must be inside a function" }
  for (i = 0; i < 2; ++i)  // { dg-error "expected|type" }
    ;
}  // { dg-error "expected" }

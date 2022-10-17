// PR c++/94376
// Like lambda-nested9.C but using explicit captures in the inner lambda.
// { dg-do compile { target c++11 } }

int main() {
  // We used to incorrectly reject the first two cases.
  int i = 0;
  [=] () {
    [i] () mutable {
      ++i;
    };
  };

#if __cpp_init_captures
  [j=0] () {
    [j] () mutable {
      ++j;
    };
  };
#endif

  [=] () {
    [&i] () mutable {
      ++i; // { dg-error "read-only" }
    };
  };

  const int j = 0;
  [=] () {
    [j] () mutable {
      ++j; // { dg-error "read-only" }
    };
  };

#if __cpp_init_captures
  [j=0] () {
    [&j] () mutable {
      ++j; // { dg-error "read-only" "" { target c++14 } }
    };
  };
#endif
}

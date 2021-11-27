// PR c++/94376
// { dg-do compile { target c++11 } }

int main() {
  // We used to incorrectly reject the first two cases.
  int i = 0;
  [=] () {
    [=] () mutable {
      ++i;
    };
  };

#if __cpp_init_captures
  [j=0] () {
    [=] () mutable {
      ++j;
    };
  };
#endif

  [=] () {
    [&] () mutable {
      ++i; // { dg-error "read-only" }
    };
  };

  const int j = 0;
  [=] () {
    [=] () mutable {
      ++j; // { dg-error "read-only" }
    };
  };

#if __cpp_init_captures
  [j=0] () {
    [&] () mutable {
      ++j; // { dg-error "read-only" "" { target c++14 } }
    };
  };
#endif
}

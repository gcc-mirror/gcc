// PR c++/67960
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Werror -fmax-errors=1" }
[[deprecated]] void doNothing(){}

int
main()
{
  doNothing(); // { dg-error "is deprecated" }
}

// { dg-message "all warnings being treated as errors" "" { target *-*-* } 0 }
// { dg-bogus "compilation terminated" "" { target *-*-* } 0 }

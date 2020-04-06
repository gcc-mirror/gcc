// PR c++/67960
// { dg-do compile }
// { dg-additional-options "-Werror -fmax-errors=1" }
__attribute__((deprecated)) void doNothing(){}

int
main()
{
  doNothing(); // { dg-error "is deprecated" }
}

// { dg-message "all warnings being treated as errors" "" { target *-*-* } 0 }
// { dg-bogus "compilation terminated" "" { target *-*-* } 0 }

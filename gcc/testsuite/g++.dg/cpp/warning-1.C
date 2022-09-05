// P2437R1 - Support for #warning
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#warning example text /* { dg-warning "example text" } */
// { dg-error "#warning before C\\\+\\\+23 is a GCC extension" "pedantic" { target c++20_down } .-1 }

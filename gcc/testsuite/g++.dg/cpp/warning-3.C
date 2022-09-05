// P2437R1 - Support for #warning
// { dg-do preprocess }
// { dg-options "" }

#warning example text /* { dg-warning "example text" } */
// { dg-bogus "#warning before C\\\+\\\+23 is a GCC extension" "" { target *-*-* } .-1 }

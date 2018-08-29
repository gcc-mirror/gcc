// Stop accepting attributes after a parenthesized initializer
// { dg-options "-fpermissive" }
int i (0) __attribute__ ((ignored)); // { dg-warning "attributes" }
// { dg-message "will be removed" "" { target *-*-* } .-1 }

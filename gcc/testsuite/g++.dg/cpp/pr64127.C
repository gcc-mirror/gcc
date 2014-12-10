/* { dg-do compile { target c++98_only } } */

template <0> int __copy_streambufs_eof; // { dg-error "" }
// { dg-error "numeric constant" "" { target *-*-* } 3 }
// { dg-warning "variable templates" "" { target *-*-* } 3 }
__copy_streambufs_eof < // { dg-error "" }
// { dg-error "parse error" "" { target *-*-* } 6 }
// { dg-error "not name a type" "" { target *-*-* } 6 }


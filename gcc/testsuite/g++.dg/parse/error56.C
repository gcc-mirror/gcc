// PR c++/64129

template <0> int __copy_streambufs_eof; // { dg-error "" }
class {
// { dg-error "forbids" "" { target *-*-* } .+1 }
    friend __copy_streambufs_eof <> ( // { dg-error "" }

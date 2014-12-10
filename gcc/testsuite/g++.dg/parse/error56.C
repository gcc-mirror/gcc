// PR c++/64129

template <0> int __copy_streambufs_eof; // { dg-error "" }
class {
    friend __copy_streambufs_eof <> ( // { dg-error "" }

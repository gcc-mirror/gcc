// { dg-do compile }

template<int I> void f1 (char [][I+1]) {}
template<int I> void f2 (char [][I+0]) {}
template<int I> void f3 (char [][I]) {}
template<int I> void f4 (char [][I-0]) {}
template<int I> void f5 (char [][I-1]) {}

template void f1 (char [][6]); // { dg-error "does not match" }
template void f2 (char [][6]); // { dg-error "does not match" }
template void f3 (char [][6]);
template void f4 (char [][6]); // { dg-error "does not match" }
template void f5 (char [][6]); // { dg-error "does not match" }

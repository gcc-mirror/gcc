/* { dg-options "-Wall" } */

extern "C" int fork (void); // { dg-warning "conflicts with built-in declaration" "" { target { *-*-solaris2* && ilp32 } } }

// PR c++/52672
// { dg-do compile }
// { dg-options "-std=c++11" }

typedef unsigned long * ul_ptr;
constexpr unsigned long a = *((ul_ptr)0x0); // { dg-error "" }
constexpr unsigned long b = *((ul_ptr)(*((ul_ptr)0x0))); // { dg-error "" }
constexpr unsigned long c = *((ul_ptr)*((ul_ptr)(*((ul_ptr)0x0)))); // { dg-error "" }

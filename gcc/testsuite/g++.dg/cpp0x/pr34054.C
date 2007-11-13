// PR c++/34054
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename... T> T foo() {} // { dg-error "not expanded|T" }

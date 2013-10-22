// PR c++/34054
// { dg-do compile }
// { dg-options "-std=c++11" }

template<typename... T> T foo() {} // { dg-error "not expanded|T" }

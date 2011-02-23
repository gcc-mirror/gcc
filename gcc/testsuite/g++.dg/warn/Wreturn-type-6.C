/* PR c++/40749 */
/* { dg-do compile } */
/* { dg-options "-Wreturn-type" } */

struct A {};
const A a() {} /* { dg-warning "no return statement" } */
const A& b() {} /* { dg-warning "no return statement" } */

const int c() {} /* { dg-warning "no return statement" } */

template<class T>
const int foo(T t) {} /* { dg-warning "no return statement" } */
int d = foo<int>(0), e = foo<int>(1);

// { dg-do compile }
// { dg-options "-finline -finline-functions" }
// Contributed by Hans Buchmann <hans dot buchmann at fhso dot ch>
// PR c++/14033: ICE while inlining a function with incomplete parameter

struct A;           // { dg-message "forward declaration" }
void foo(A a) {}    // { dg-error "incomplete" }
struct A {};

void bar(void)
{
 foo(A());
}

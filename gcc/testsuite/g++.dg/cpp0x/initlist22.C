// Core issue 934/1288
// { dg-do compile { target c++11 } }

int i;

int& r1{ i };			// OK, direct binding
int&& r2{ i };			// { dg-error "" } binding && to lvalue

int& r3{ };			// { dg-error "" } reference to temporary
int&& r4{ };			// OK, reference to temporary

struct A { int i; } a;

A& r5 { i };			// { dg-error "" } reference to temporary
A&& r6 { i };			// OK, aggregate initialization of temporary
A& r7 { a };			// OK, direct-initialization
A&& r8 { a };			// { dg-error "lvalue" } binding && to lvalue

struct B { B(int); int i; } b(0);

B& r9 { i };			// { dg-error "" } reference to temporary
B&& r10 { i };			// OK, make temporary with B(int) constructor
B& r11 { b };			// OK, direct-initialization
B&& r12 { b };			// { dg-error "lvalue" } binding && to lvalue

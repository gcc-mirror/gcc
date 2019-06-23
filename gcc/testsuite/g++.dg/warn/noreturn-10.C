// PR c++/60364
// { dg-do compile { target c++11 } }

void* fn1 [[gnu::returns_twice, noreturn]] (); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'returns_twice'" }
void* fn2 [[gnu::alloc_align(1), noreturn]] (int); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'alloc_align'" }
void* fn3 [[gnu::alloc_size(1), noreturn]] (int); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'alloc_size'" }
void* fn4 [[gnu::const, noreturn]] (); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'const'" }
void* fn5 [[gnu::malloc, noreturn]] (int); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'malloc'" }
void* fn6 [[gnu::pure, noreturn]] (); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'pure'" }
void* fn7 [[gnu::warn_unused_result, noreturn]] (); // { dg-warning "ignoring attribute 'noreturn' because it conflicts with attribute 'warn_unused_result'" }

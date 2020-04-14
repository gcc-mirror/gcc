// { dg-do compile }

import gcc.attributes;

@alloc_size(1)
int ignoredfunc(int size); // { dg-warning ".alloc_size. attribute ignored on a function returning .int." }

@alloc_size(0) int var; // { dg-warning ".alloc_size. attribute only applies to function types" }

@attribute("alloc_size", "1")
void* invalid1(int size); // { dg-warning ".alloc_size. attribute argument is invalid" }

@attribute("alloc_size", 1, "2")
void* invalid2(int count, int size); // { dg-warning ".alloc_size. attribute argument 2 is invalid" }

@attribute("alloc_size", 0.1)
void* wrongtype1(int size); // { dg-warning ".alloc_size. attribute argument has type .double." }

@attribute("alloc_size", 1, 0.2)
void* wrongtype2(int count, int size); // { dg-warning ".alloc_size. attribute argument 2 has type .double." }

@alloc_size(0)
void* malloc0(int size); // { dg-warning ".alloc_size. attribute argument value .0. does not refer to a function parameter" }

@alloc_size(1, 0)
void* malloc0(int count, int size); // { dg-warning ".alloc_size. attribute argument 2 value .0. does not refer to a function parameter" }

@alloc_size(1, 0, true)
void* malloc0pos(int count, int size);

@alloc_size(1, -1, true)
void* mallocminus1(int count, int size); // { dg-warning ".alloc_size. attribute argument 2 value .-1. does not refer to a function parameter" }

@alloc_size(99)
void* malloc99(int size); // { dg-warning ".alloc_size. attribute argument value .99. exceeds the number of function parameters 1" }

@alloc_size(1, 99)
void* malloc99(int count, int size); // { dg-warning ".alloc_size. attribute argument 2 value .99. exceeds the number of function parameters 2" }

@alloc_size(1)
void* mallocdouble(double size); // { dg-warning ".alloc_size. attribute argument value .1. refers to parameter type .double." }

@alloc_size(2, 1)
void* mallocdouble(int count, double size); // { dg-warning ".alloc_size. attribute argument 1 value .2. refers to parameter type .double." }

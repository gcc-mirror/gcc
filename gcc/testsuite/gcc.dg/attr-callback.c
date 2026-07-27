/* Test callback attribute error checking. */

/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wattributes" } */

[[gnu::callback_only(1, 2)]]
void
correct_1(void (*)(int*), int*);

[[gnu::callback_only(1, 2, 3)]]
void
correct_2(void (*)(int*, double*), int*, double*);

[[gnu::callback_only(1, 2, 3), gnu::callback_only(4, 5)]]
void
correct_3(void (*)(int*, double*), int*, double*, int (*)(void*), void*);

[[gnu::callback_only(1, 0)]]
void
unknown_1(void (*)(int*));

[[gnu::callback_only(1, 2, 0)]]
void
unknown_2(void (*)(int*, double*), int*, double*, char*);

[[gnu::callback_only(1, 0, 3, 3)]]
void
too_many(void (*)(int*, double*), int*, double*); /* { dg-error "argument number mismatch, 2 expected, got 3" }*/

[[gnu::callback_only(1, 2)]]
void
too_few_1(void (*)(int*, double*), int*, double*); /* { dg-error "argument number mismatch, 2 expected, got 1" }*/

[[gnu::callback_only(1)]]
void
too_few_2(void (*)(int*, double*), int*, double*); /* { dg-error "argument number mismatch, 2 expected, got 0" }*/

[[gnu::callback_only(3, 1)]]
void
promotion(char*, float, int (*)(int*));

[[gnu::callback_only(2, 3)]]
void
downcast(char*, void* (*)(float*), double*);

[[gnu::callback_only(1, 2, 5)]]
void
out_of_range_1(char (*)(float*, double*), float*, double*, int*); /* { dg-error "callback argument index 5 is out of range" } */

[[gnu::callback_only(1, -2, 3)]]
void
out_of_range_2(char (*)(float*, double*), float*, double*, int*); /* { dg-error "callback argument index -2 is out of range" } */

[[gnu::callback_only(-1, 2, 3)]]
void
out_of_range_3(char (*)(float*, double*), float*, double*, int*); /* { dg-error "callback function index -1 is out of range" } */

[[gnu::callback_only(67, 2, 3)]]
void
out_of_range_4(char (*)(float*, double*), float*, double*, int*); /* { dg-error "callback function index 67 is out of range" } */

[[gnu::callback_only(0, 2, 3)]]
void
unknown_fn(char (*)(float*, double*), float*, double*, int*); /* { dg-error "callback function position cannot be marked as unknown" } */

[[gnu::callback_only(1, 2)]]
void
not_a_fn(int, int); /* { dg-error "argument no. 1 is not an address of a function" } */

struct S
{
  int x;
};

[[gnu::callback_only(1, 2)]]
void
incompatible_types_1(void (*)(struct S*), struct S); /* { dg-error "argument type at index 2 is not compatible with callback argument type at index 1" } */

[[gnu::callback_only(1, 3, 2)]]
void
incompatible_types_2(void (*)(struct S*, int*), int*, double); /* { dg-error "argument type at index 3 is not compatible with callback argument type at index 1" } */

[[gnu::callback_only(1, "2")]]
void
wrong_arg_type_1(void (*)(void*), void*); /* { dg-error "argument no. 1 is not an integer constant" } */

[[gnu::callback_only("not a number", 2, 2)]]
void
wrong_arg_type_2(void (*)(void*, void*), void*); /* { dg-error "argument specifying callback function position is not an integer constant" } */

[[gnu::callback_only(1, 2), gnu::callback_only(1, 3)]]
void
multiple_single_fn(void (*)(int*), int*, int*); /* { dg-error "function declaration has multiple callback attributes describing argument no. 1" } */

/* Check that the attribute won't resolve outside of our namespace.  */

[[callback(1, 2)]] /* { dg-warning "ignored" } */
void
ignore_1(void (*)(int*), int*);

[[gnu::callback(1, 2)]]
void
ignore_2(void (*)(int*), int*); /* { dg-warning "ignored" } */

[[clang::callback_only(1, 2)]]
void
ignore_3(void (*)(int*), int*); /* { dg-warning "ignored" } */

// { dg-options "-std=c++11 -Wunused -pedantic-errors" }
// { dg-do compile }
// Test for syntax support of various attribute permutations.

int
[[gnu::noreturn]] // { dg-warning "ignored" }
one
[[gnu::unused]]
(void);

int one_third [[gnu::noreturn]] [[gnu::unused]] (void);

int [[gnu::unused]] one_half(); // { dg-warning "ignored" }

static
[[gnu::noreturn]] // { dg-warning "ignored" }
void two [[gnu::unused]] (void) {}



[[gnu::unused]]
int
five(void)
[[gnu::noreturn]] // { dg-warning "ignored" }
{}

[[gnu::noreturn]]
void
six (void)
;

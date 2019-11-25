// { dg-options "-Wunused -pedantic-errors" }
// { dg-do compile { target c++11 } }
// Test for syntax support of various attribute permutations.

int
[[noreturn]] // { dg-warning "ignored" }
one
[[gnu::unused]]
(void);

int one_third [[noreturn]] [[gnu::unused]] (void);

int [[gnu::unused]] one_half(); // { dg-warning "ignored" }

static
[[noreturn]] // { dg-warning "ignored" }
void two [[gnu::unused]] (void) {}



[[gnu::unused]]
int
five(void)
[[noreturn]] // { dg-warning "does not apply to types" }
{ return 0; }

[[noreturn]]
void
six (void)
;

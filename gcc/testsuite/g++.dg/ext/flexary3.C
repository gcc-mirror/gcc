// PR c++/54441 - [4.7/4.8 Regression] Infinite loop with brace initializer
//                on zero-length array
// Note that although the definition of struct s in the test case for
// c++/54441 was accepted as valid, it is, in fact, invalid in C, and
// as noted in c++/42121, should be treated as invalid in C++ as well.
// The test verifies that gcc detects, reports, and handles both errors
// gracefully.
// Note also that the error(s) issued for the invalid initializer depend
// on c++/55606.

// { dg-options "" }

struct s {
    char c[];   // { dg-error "flexible array member .* in an otherwise empty" }
};

int main()
{
    struct s s = { .c = 0 };	// { dg-error "initializer" }
    return 0;
}

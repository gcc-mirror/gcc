// Test for syntax support of various attribute permutations.

int
__attribute__((noreturn))
__attribute__((unused))
one(void); // OK 

__attribute__((noreturn))
__attribute__((unused))
int
two(void); // OK

int
__attribute__((unused))
three (void)
__attribute__((noreturn)); // OK

__attribute__((unused))
int
four (void)
__attribute__((noreturn)); // OK

int
five(void)
__attribute__((noreturn))
__attribute__((unused));  // OK

__attribute__((noreturn))
int
__attribute__((unused)) // parse error before '__attribute__' in C++
six (void);              // OK in C

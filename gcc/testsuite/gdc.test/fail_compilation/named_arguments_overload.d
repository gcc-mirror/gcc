/**
TEST_OUTPUT:
---
fail_compilation/named_arguments_overload.d(33): Error: none of the overloads of `snoopy` are callable using argument types `(immutable(S), immutable(T))`
fail_compilation/named_arguments_overload.d(17):        Candidates are: `named_arguments_overload.snoopy(S s, int i = 0, T t = T())`
fail_compilation/named_arguments_overload.d(18):                        `named_arguments_overload.snoopy(T t, int i, S s)`
fail_compilation/named_arguments_overload.d(34): Error: none of the overloads of `snoopy` are callable using argument types `(immutable(T), immutable(S))`
fail_compilation/named_arguments_overload.d(17):        Candidates are: `named_arguments_overload.snoopy(S s, int i = 0, T t = T())`
fail_compilation/named_arguments_overload.d(18):                        `named_arguments_overload.snoopy(T t, int i, S s)`
fail_compilation/named_arguments_overload.d(35): Error: `named_arguments_overload.snoopy` called with argument types `(immutable(S), immutable(T), immutable(int))` matches both:
fail_compilation/named_arguments_overload.d(17):     `named_arguments_overload.snoopy(S s, int i = 0, T t = T())`
and:
fail_compilation/named_arguments_overload.d(18):     `named_arguments_overload.snoopy(T t, int i, S s)`
---
*/

char snoopy(S s, int i = 0, T t = T.init) { return 'B'; }
char snoopy(T t, int i, S s) { return 'A'; }

struct S { }
struct T { }
immutable S s = S.init;
immutable T t = T.init;
immutable int i = 0;

static assert(snoopy(t,   i, s    ) == 'A');
static assert(snoopy(s,   i, t    ) == 'B');
static assert(snoopy(s:s, t:t     ) == 'B');
static assert(snoopy(t:t, s:s     ) == 'B');
static assert(snoopy(t:t, i,   s:s) == 'A');
static assert(snoopy(s:s, t:t, i  ) == 'A');

immutable err0 = snoopy(s, t); // error, neither A nor B match
immutable err1 = snoopy(t, s); // error, neither A nor B match
immutable err2 = snoopy(s:s, t:t, i:i); // error, ambiguous

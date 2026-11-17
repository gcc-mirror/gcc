// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

struct S { int s; int foo (int); };
struct D : S {};
SA (__is_invocable (int S::*, S(&)[1]));
SA (__is_invocable (int S::*, S(&)[2]));
SA (__is_invocable (int S::*, S(&)[]));
SA (__is_invocable (int S::*, S[1]));
SA (__is_invocable (int S::*, S[2]));
SA (__is_invocable (int S::*, S[]));
SA (__is_invocable (int S::*, D(&)[1]));
SA (__is_invocable (int S::*, D(&)[2]));
SA (__is_invocable (int S::*, D(&)[]));
SA (__is_invocable (int S::*, D[1]));
SA (__is_invocable (int S::*, D[2]));
SA (__is_invocable (int S::*, D[]));
SA (__is_invocable (int (S::*) (int), S(&)[1], int));
SA (__is_invocable (int (S::*) (int), S(&)[2], int));
SA (__is_invocable (int (S::*) (int), S(&)[], int));
SA (__is_invocable (int (S::*) (int), S[1], int));
SA (__is_invocable (int (S::*) (int), S[2], int));
SA (__is_invocable (int (S::*) (int), S[], int));
SA (__is_invocable (int (S::*) (int), D(&)[1], int));
SA (__is_invocable (int (S::*) (int), D(&)[2], int));
SA (__is_invocable (int (S::*) (int), D(&)[], int));
SA (__is_invocable (int (S::*) (int), D[1], int));
SA (__is_invocable (int (S::*) (int), D[2], int));
SA (__is_invocable (int (S::*) (int), D[], int));

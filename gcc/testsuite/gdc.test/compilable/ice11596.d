// PERMUTE_ARGS: -inline -release -g -O -version=X

version(X)
    alias M = real;
else
    alias M = int[2]; /* or other T[n] with n != 1 */

struct S { M m; }

S f() { assert(false); }

class C
{
    S[1] ss; /* Here, size doesn't matter. */

    this() { ss[] = f(); }
}

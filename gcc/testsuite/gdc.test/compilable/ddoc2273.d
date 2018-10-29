// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 2273
// REQUIRED_ARGS: -m32

module ddoc2273;

interface A { }

interface C { }
interface D { }

///
interface B : C, D { }

///
class Foo : A, B { }

///
MinType!(T1, T2, T) min(T1, T2, T...)(T1 a, T2 b, T xs) { }

///
Templ!([1, 2, 3]) max(T...)() { }

///
template Base64Impl(char Map62th, char Map63th, char Padding) { }

///
int sqlite3_config(int,...);

template staticIndexOf(T, TList...) { alias int staticIndexOf; }

///
alias staticIndexOf IndexOf;

void main() { }


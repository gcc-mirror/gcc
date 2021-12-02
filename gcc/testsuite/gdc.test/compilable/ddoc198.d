// EXTRA_SOURCES: extra-files/ddoc198.ddoc
// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc198;

///
interface I1 { }

///
class C1 { }

///
class Foo : C1, I1 { }

///
enum X { x = 1 }

///
enum Y : X { y = X.x }

///
struct S1 { }

///
enum enS : S1 { a = S1() }

// disabled until class enums are possible
// enum enC : C1 { a = new C1() }


void main()
{
}

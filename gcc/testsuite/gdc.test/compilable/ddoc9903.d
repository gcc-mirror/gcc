// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 9903

/// sss
struct S9903X {}
/// Ditto
struct S9903Y {}

/// ccc
class C9903X {}
/// Ditto
class C9903Y {}

/// uuu
union U9903X {}
/// Ditto
union U9903Y {}

/// iii
interface I9903X {}
/// Ditto
interface I9903Y {}

/// eee
enum E9903X { a }
/// Ditto
enum E9903Y { a }

///
enum {
    a9903,  /// ea
    b9903,  /// Ditto
    c9903,  /// ec
}

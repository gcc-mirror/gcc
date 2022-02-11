// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
// REQUIRED_ARGS: -m32
// EXTRA_SOURCES: extra-files/ddoc10367.ddoc

module ddoc10367;

/// A
enum A
{
    a = 1, /// a
    b = 2  /// b
}

/// B
enum B : long
{
    a = 1, /// a
    b = 2  /// b
}

/// C
enum C : string
{
    a = "a", /// a
    b = "b"  /// b
}

// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

//-----------------------------------------------
/// my enum
enum E1
{
    A, /// element a
    B  /// element b
}

/// my enum
enum E2
{
    /// element a
    A,
    /// element b
    B
}

/// my enum
enum E3
{
      A /// element a
    , B /// element b
}

/// my enum
enum E4
{
    A /// element a
    ,
    B /// element b
}

/// my enum
enum E5
{
    /// element a
    A
    ,
    /// element b
    B
}

/// Some doc
void foo() {}

/// More doc
alias foo bar;

/// asdf
class C
{
    /// Some doc
    abstract void foo();
}


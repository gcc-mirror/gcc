// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc648;

/// Mixin declaration
mixin template Mixin1()
{
    /// struct S
    struct S { }
}

/// class A
class A
{
    /// field x
    int x;

    /// no docs for mixin statement (only for expanded members)
    mixin Mixin1!();
}

/// class AB
class AB
{
    /// field x
    int x;

    // no docs for mixin or its contents, must be a ddoc comment
    mixin Mixin1!();
}

/// Mixin declaration2
mixin template Mixin2()
{
    /// struct S2
    struct S2 { }
}

/// Mixin declaration3
mixin template Mixin3()
{
    /// another field
    int f;

    /// no docs for mixin statement (only for expanded members)
    mixin Mixin2!();
}

/// class B1
class B1
{
    /// no docs for mixin statement (only for expanded members)
    mixin Mixin3!();
}


/// Mixin declaration3
mixin template Mixin4()
{
    /// another field
    int f;

    // no docs at all for non-ddoc comment
    mixin Mixin2!();
}

/// class B2
class B2
{
    /// no docs for mixin statement (only for expanded members)
    mixin Mixin4!();
}

/// no docs for mixin statement (only for expanded members)
mixin Mixin3!();

///
struct TS(T)
{
    mixin template MT()
    {
    }

    mixin MT;  /// avoid calling semantic

    ///
    int field;
}

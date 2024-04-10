// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// EXTRA_FILES: ddoc5446a.d ddoc5446b.d
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
module ddoc5446;
import ddoc5446a;
private import ddoc5446b;

/** */
alias A_Foo This_Foo;

/** */
alias A_Foo_Alias This_Foo_Alias;

/** */
alias int This_Int;

/** */
alias A_Enum This_Enum;

/** */
deprecated alias ddoc5446b.A_Enum_New A_Enum_New;

struct Nested
{
}

/** */
struct Bar
{
    /** */
    alias A_Foo Bar_A_Foo;

    /** */
    alias A_Foo_Alias Bar_A_Foo_Alias;

    /** */
    alias A_Int Bar_A_Int;

    /** */
    alias This_Foo Bar_This_Foo;

    /** */
    alias This_Foo_Alias Bar_This_Foo_Alias;

    /** */
    alias This_Int Bar_This_Int;

    /** */
    alias Nested Nested_Alias;

    /** */
    alias .Nested Fake_Nested;

    /** */
    struct Nested
    {
        /** */
        alias Bar Bar_Nested_Bar_Alias;

        /** */
        alias .Bar Bar_Alias;

        /** */
        struct Bar
        {

        }
    }
}

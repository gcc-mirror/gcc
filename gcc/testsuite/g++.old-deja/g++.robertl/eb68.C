
// Error: Internal Compiler Error.

        class foo {
                typedef int an_int;
        };
        class bar : foo::an_int {};  // causes internal compiler error

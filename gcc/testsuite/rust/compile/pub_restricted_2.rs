// { dg-additional-options "-w" }

mod foo {
    mod bar {
        mod baz {
            pub(in baz) struct A0;
            pub(in bar::baz) struct A1;
            pub(in foo::bar::baz) struct A2;

            mod sain {
                mod doux {}
            }

            pub(in sain) struct A3; // { dg-error "restricted path is not an ancestor of the current module" }
            pub(in sain::doux) struct A4; // { dg-error "restricted path is not an ancestor of the current module" }
        }
    }
}

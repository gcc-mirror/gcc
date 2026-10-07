#![feature(no_core)]
#![no_core]

// Adapted from rustc's tests/ui/pattern/issue-6449.rs, which used to crash
// the exhaustiveness check. The `..` rest patterns of the original are spelled
// out with wildcards here.

enum Foo {
    Bar(isize),
    Baz,
}

enum Other {
    Other1(Foo),
    Other2(Foo, Foo),
}

fn main() {
    match Foo::Baz {
        crate::Foo::Bar(3) => {}
        crate::Foo::Bar(_) if false => {}
        crate::Foo::Bar(_) if false => {}
        crate::Foo::Bar(_n) => {}
        crate::Foo::Baz => {}
    }

    match Other::Other1(Foo::Baz) {
        crate::Other::Other1(crate::Foo::Baz) => {}
        crate::Other::Other1(crate::Foo::Bar(_)) => {}
        crate::Other::Other2(crate::Foo::Baz, crate::Foo::Bar(_)) => {}
        crate::Other::Other2(crate::Foo::Bar(_), crate::Foo::Baz) => {}
        crate::Other::Other2(_, _) => {}
    }
}

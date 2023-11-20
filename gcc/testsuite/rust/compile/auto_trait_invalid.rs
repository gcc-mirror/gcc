// #![feature(auto_traits)] // not present in Rust 1.49 yet

#![feature(optin_builtin_traits)]

auto trait Invalid {
    // { dg-error "auto traits cannot have methods or associated items" "" { target *-*-* } .-1 }

    fn foo(); // { dg-message "remove this item" }

    fn bar() {} // { dg-message "remove this item" }

    type Foo; // { dg-message "remove this item" }

    const FOO: i32; // { dg-message "remove this item" }

    const BAR: i32 = 15; // { dg-message "remove this item" }
}

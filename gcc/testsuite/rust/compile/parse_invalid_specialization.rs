default fn f() {
    // { dg-error ".default. is only allowed on items within .impl. blocks" "" { target *-*-* } .-1 }
    // { dg-error "failed to parse item in crate" "" { target *-*-* } .-2 }
}

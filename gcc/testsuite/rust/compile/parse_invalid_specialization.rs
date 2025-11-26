default fn f() {
    // { dg-error ".default. is only allowed on items within .impl. blocks" "" { target *-*-* } .-1 }
}

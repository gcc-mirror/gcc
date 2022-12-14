fn foo (x: i8) -> i32 { // { dg-warning "function is never used" }
    //
    match x {
        1 => { return 1; }
        _ => { return 0; }
    }
}

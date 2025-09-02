pub macro m($inner_str:expr) {
    #[m = $inner_str] 
    // { dg-error "macro not found" "" { target *-*-* } .-1 }

    struct S;
}

fn main() {
    m!(stringify!(foo));
}

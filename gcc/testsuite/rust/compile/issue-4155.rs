struct Bug {
    inner: [(); match Vec::new {
        f @  |n() => 1
// { dg-error "failed to parse pattern to bind" "" { target *-*-* } .-1 }
// { dg-error "unexpected token .|. in pattern" "" { target *-*-* } .-2 }
    }],
}

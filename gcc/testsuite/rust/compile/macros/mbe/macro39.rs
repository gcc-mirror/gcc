macro_rules! m {
    ($e:expr (, parenthesis_forbidden)) => {{}}; // { dg-error "token .\\(. at start of matcher is not allowed after .expr. fragment" }
                                                 // { dg-error "required first macro rule" "" { target *-*-* } .-1 }
}

macro_rules! forbidden_frag {
    ($t:ty $not_block:ident) => {{}}; // { dg-error "fragment specifier .ident. is not allowed after .ty. fragments" }
                                      // { dg-error "required first macro rule in macro rules definition could not be parsed" "" { target *-*-* } .-1 }
                                      // { dg-error "failed to parse item in crate" "" { target *-*-* } .-2 }
}

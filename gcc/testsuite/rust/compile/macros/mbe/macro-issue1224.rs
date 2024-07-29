macro_rules! impl_uint {
    ($($ty:ident),*) => {
        impl $ty {} // { dg-error "metavariable is still repeating at this depth" }
                    // { dg-error "unrecognised token" "" { target *-*-* } .-1 } // Spurious
                    // { dg-error "could not parse type" "" { target *-*-* } .-2 } // Spurious
    };
}

impl_uint!(u8, u16, u32, u64, u128);

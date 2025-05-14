// { dg-additional-options "-frust-cfg=A -w" }

macro_rules! impl_uint {
    ($($ty:ident = $lang:literal),*) => {
        $(
            impl $ty {
                pub fn to_le(self) -> Self {
                    #[cfg(not(A))]
                    {
                        self
                    }
                    #[cfg(A)]
                    {
                        self
                    }
                }
            }
        )*
    }
}

impl_uint!(u8 = "u8", u16 = "u16", u32 = "u32");

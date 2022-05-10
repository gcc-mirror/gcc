// { dg-additional-options "-w" }

macro_rules! impl_uint {
    ($($ty:ident = $lang:literal),*) => {
        $(
            impl $ty {
                pub fn to_le(self) -> Self {
                    #[cfg(not(target_endian = "little"))]
                    {
                        self
                    }
                    #[cfg(target_endian = "little")]
                    {
                        self
                    }
                }
            }
        )*
    }
}

impl_uint!(u8 = "u8", u16 = "u16", u32 = "u32");

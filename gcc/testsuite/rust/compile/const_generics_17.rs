struct Foo<const N: u32 = 1, const O: bool>; // { dg-error "invalid order for generic parameters: generic parameters with a default must be trailing" }

impl<const N: u32> Foo<N> {}

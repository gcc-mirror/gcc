trait MemoryUnit {
    extern "C" fn read_dword(&'s self) -> u16 {}
    // { dg-error {failed to resolve lifetime} "" { target *-*-* } .-1 }
    // { dg-error {mismatched types} "" { target *-*-* } .-2 }
}

impl MemoryUnit for MemoryUnit {
    extern "C" fn read_dword(&'s self) -> u16 {
        let b16 = self.read_word() as u16;

        b16 << 8
    }
}

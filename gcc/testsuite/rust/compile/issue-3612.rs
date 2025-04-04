trait _St1 {
    pub const UNDERFLOW: *const u16 = unsafe { [0u16; 1].as_ptr().offset(isize::MIN) };
    // { dg-error "no method named .as_ptr. found in the current scope .E0599." "" { target *-*-* } .-1 }
    // { dg-error "failed to resolve receiver in MethodCallExpr" "" { target *-*-* } .-2 }
}

fn main() {}

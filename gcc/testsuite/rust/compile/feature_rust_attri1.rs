
pub struct NotI8(i8);

impl NotI8 {
    #[rustc_inherit_overflow_checks] //{ dg-error "internal implementation detail" "" { target *-*-* }  }
    pub fn add(self, other: NotI8) -> NotI8 {
        NotI8(self.0 + other.0)
    }
}

fn main() -> i32 {
    0
}

// { dg-output "1\r*\n" }
// { dg-additional-options "-w" }
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "bitand_assign"]
pub trait BitAndAssign<Rhs = Self> {
    fn bitand_assign(&mut self, rhs: Rhs);
}

impl BitAndAssign for i32 {
    fn bitand_assign(&mut self, other: i32) {
        *self &= other;

        unsafe {
            let a = "%i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, *self);
        }
    }
}

fn main() -> i32 {
    let mut a = 1;
    a &= 1;

    0
}

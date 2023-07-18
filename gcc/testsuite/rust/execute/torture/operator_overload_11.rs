// { dg-output "1\r*\n" }
// { dg-additional-options "-w" }
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "bitand"]
pub trait BitAnd<Rhs = Self> {
    type Output;

    fn bitand(self, rhs: Rhs) -> Self::Output;
}

impl BitAnd for i32 {
    type Output = i32;

    fn bitand(self, other: i32) -> i32 {
        let res = self & other;

        unsafe {
            let a = "%i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, res);
        }

        res
    }
}

fn main() -> i32 {
    let a;
    a = 1 & 1;

    0
}

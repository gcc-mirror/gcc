/* { dg-output "not\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "not"]
pub trait Not {
    type Output;

    fn not(self) -> Self::Output;
}

impl Not for i32 {
    type Output = i32;

    fn not(self) -> i32 {
        unsafe {
            let a = "not\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
        !self
    }
}

fn main() -> i32 {
    let a: i32 = 1;
    let _b = !a;

    0
}

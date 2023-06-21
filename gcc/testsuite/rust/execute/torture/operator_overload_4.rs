/* { dg-output "neg\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "neg"]
pub trait Neg {
    type Output;

    fn neg(self) -> Self::Output;
}

impl Neg for i32 {
    type Output = i32;

    fn neg(self) -> i32 {
        unsafe {
            let a = "neg\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
        -self
    }
}

fn main() -> i32 {
    let a: i32 = 1;
    let _b = -a;

    0
}

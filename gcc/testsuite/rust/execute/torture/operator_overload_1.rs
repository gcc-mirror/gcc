/* { dg-output "3\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "add"]
pub trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
    // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .rhs." "" { target *-*-* } .-2 }
    // { dg-warning "unused name .Add::add." "" { target *-*-* } .-3 }
}

impl Add for i32 {
    type Output = i32;

    fn add(self, other: i32) -> i32 {
        let res = self + other;

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
    a = 1 + 2;

    0
}

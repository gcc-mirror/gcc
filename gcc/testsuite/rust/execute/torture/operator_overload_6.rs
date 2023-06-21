/* { dg-output "add_assign\r*\n3\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "add_assign"]
pub trait AddAssign<Rhs = Self> {
    fn add_assign(&mut self, rhs: Rhs);
}

impl AddAssign for i32 {
    fn add_assign(&mut self, other: i32) {
        unsafe {
            let a = "add_assign\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
        *self += other
    }
}

fn main() -> i32 {
    let mut res = 1;
    res += 2;

    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, res);
    }

    0
}

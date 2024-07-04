/* { dg-output "mut_deref\r*\n123\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "deref"]
pub trait Deref {
    type Target;

    fn deref(&self) -> &Self::Target;
}

impl<T> Deref for &T {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            let a = "imm_deref\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }

        *self
    }
}

impl<T> Deref for &mut T {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            let a = "mut_deref\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }

        *self
    }
}

fn main() -> i32 {
    let foo = &mut 123;
    let res: i32 = *foo;

    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, res);
    }

    0
}

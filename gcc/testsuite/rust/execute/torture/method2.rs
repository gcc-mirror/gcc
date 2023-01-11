// { dg-additional-options "-w" }
// { dg-output "foo_deref\r*\nimm_deref\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

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

struct Bar(i32);
impl Bar {
    fn foobar(self) -> i32 {
        self.0
    }
}

struct Foo<T>(T);
impl<T> Deref for Foo<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let a = "foo_deref\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }

        &self.0
    }
}

pub fn main() -> i32 {
    let bar = Bar(123);
    let foo: Foo<&Bar> = Foo(&bar);
    let foobar: i32 = foo.foobar();

    foobar - 123
}

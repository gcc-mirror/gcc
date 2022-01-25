// { dg-additional-options "-w" }
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

struct Bar(i32);
impl Bar {
    fn cake(self) -> i32 {
        self.0 + 1
    }
}

pub fn main() {
    let foo: Foo<Bar> = Foo(Bar(123));
    let bar: Bar = *foo;

    let cake_result: i32 = foo.cake();
}

// { dg-additional-options "-w" }
// { dg-output "mut_deref\r*\nfoobar: 123\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "deref"]
pub trait Deref {
    type Target;

    fn deref(&self) -> &Self::Target;
}

#[lang = "deref_mut"]
pub trait DerefMut: Deref {
    fn deref_mut(&mut self) -> &mut Self::Target;
}

impl<T> Deref for &T {
    type Target = T;

    fn deref(&self) -> &T {
        *self
    }
}

impl<T> Deref for &mut T {
    type Target = T;
    fn deref(&self) -> &T {
        *self
    }
}

pub struct Bar(i32);
impl Bar {
    pub fn foobar(&mut self) -> i32 {
        self.0
    }
}

pub struct Foo<T>(T);
impl<T> Deref for Foo<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Foo<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let a = "mut_deref\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }

        &mut self.0
    }
}

pub fn main() -> i32 {
    let mut bar = Bar(123);
    let mut foo: Foo<&mut Bar> = Foo(&mut bar);
    let foobar = foo.foobar();

    unsafe {
        let a = "foobar: %i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, foobar);
    }

    foobar - 123
}

/* { dg-output "0\r*\n2\r*\n" } */
#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

enum BookFormat {
    Paperback,
    Hardback,
    Ebook,
}

extern "C" {
    fn printf(s: *const i8, ...);
}

mod core {
    mod intrinsics {
        #[lang = "discriminant_kind"]
        pub trait DiscriminantKind {
            #[lang = "discriminant_type"]
            type Discriminant;
        }

        extern "rust-intrinsic" {
            pub fn discriminant_value<T>(v: &T) -> <T as DiscriminantKind>::Discriminant;
        }
    }
}

pub fn main() -> i32 {
    let a = BookFormat::Paperback;
    let b = BookFormat::Ebook;

    unsafe {
        let val1: isize = core::intrinsics::discriminant_value(&a);
        let val2 = core::intrinsics::discriminant_value(&b);

        let a = "%i\n";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, val1 as i32);
        printf(c, val2 as i32);
    }

    0
}

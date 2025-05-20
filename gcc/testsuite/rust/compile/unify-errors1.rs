#[lang = "sized"]
trait Sized {}

#[lang = "copy"]
trait Copy {}

trait MyTrait {}

struct Wrapper<T: MyTrait> {
    value: T,
}

struct NotImpl;

trait A {}
trait B {}

struct Wrapper2<T: A + B> {
    value: T,
}

struct NotImpl2;

impl A for NotImpl2 {}

fn takes_tuple(x: (i32, bool)) {}

fn requires_copy<T: Copy>(value: T) {}

pub fn test() {
    takes_tuple((1, 2));
    // { dg-error "mismatched types, expected .bool. but got .<integer>. .E0308." "" { target *-*-* } .-1 }

    takes_tuple((1, 2, 3));
    // { dg-error "mismatched types, expected ..i32, bool.. but got ..<integer>, <integer>, <integer>.. .E0308." "" { target *-*-* } .-1 }

    takes_tuple("hello");
    // { dg-error "mismatched types, expected ..i32, bool.. but got .& str. .E0308." "" { target *-*-* } .-1 }

    let x = &mut 5;
    requires_copy(x);
    // { dg-error "bounds not satisfied for &mut <integer> .Copy. is not satisfied .E0277." "" { target *-*-* } .-1 }

    let _x = Wrapper { value: NotImpl };
    // { dg-error "bounds not satisfied for NotImpl .MyTrait. is not satisfied .E0277." "" { target *-*-* } .-1 }

    let _x = Wrapper2 { value: NotImpl2 };
    // { dg-error "bounds not satisfied for NotImpl2 .B. is not satisfied .E0277." "" { target *-*-* } .-1 }
}

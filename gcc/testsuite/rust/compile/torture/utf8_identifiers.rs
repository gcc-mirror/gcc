#[lang = "sized"]
pub trait Sized {}

pub fn f<'かに>() {
    let crab = ();

    let Κάβουρας = 0.001;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let 게 = "";
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let سلطعون = 0.;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let _: &'かに () = &crab;
}

pub fn g<'β, γ>() {}

struct _S {
    δ: i32
}

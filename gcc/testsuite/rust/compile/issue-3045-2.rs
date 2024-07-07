#![feature(dropck_eyepatch)]
#[allow(dead_code)]

#[lang = "sized"]
trait Sized {}


trait Action {}

struct Inspector<'a>(&'a u8);
struct Inspector2<'a>(&'a u8);

impl<#[may_dangle] 'a> Action for Inspector<'a> {} // { dg-error "use of 'may_dangle' is unsafe and requires unsafe impl" "" { target *-*-* } 0 }

unsafe impl<#[may_dangle] 'a> Action for Inspector2<'a> {}


fn main() {

}

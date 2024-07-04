// { dg-additional-options "-w" }

#[lang = "sized"]
pub trait Sized {}

struct Adt;
enum EAdt {
    V0,
    V1,
}
struct Registers {
    r0: i64,
    r1: i64,
    r2: i64,
    r3: i64,
}
trait Foo {}

fn foo1(value: bool) {}
fn foo2(value: char) {}
fn foo3(value: i32) {}
fn foo4(value: u16) {}
fn foo5(value: f64) {}
fn foo6(value: usize) {}
fn foo7(value: isize) {}
fn foo8(value: Adt) {}
fn foo9(value: EAdt) {}
fn foo10(value: &str) {}
fn foo11(value: *const i8) {}
fn foo12<T>(value: T) {}
fn foo13(value: [i32; 5]) {}
fn foo14(value: [Adt]) {}
fn foo15(value: fn(i32) -> i32) {}
fn foo16(value: (i32, Adt)) {}
fn foo17(value: (i32, [f64; 5])) {}
fn foo18(value: Registers) {}
fn foo19(value: &dyn Foo) {}
fn foo20(value: &[Adt]) {}
fn foo21(value: fn(i32)) {}
fn foo22(value: fn()) {}
fn foo23(value: fn() -> i32) {}

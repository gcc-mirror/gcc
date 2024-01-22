// { dg-output "hello world: gccrs\r*\n" }
// { dg-additional-options "-w" }
static TEST_1: &str = "gccrs";
static TEST_2: i32 = 123;

struct Foo(i32, bool);
static TEST_3: Foo = Foo(123, false);

extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    unsafe {
        let a1 = "hello world: %s\n";
        let b1 = a1 as *const str;
        let c1 = b1 as *const i8;

        let a2 = TEST_1;
        let b2 = a2 as *const str;
        let c2 = b2 as *const i8;

        printf(c1, c2);
    }
    0
}

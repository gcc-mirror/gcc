/* { dg-output "bar foo baz foobar\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
    fn memchr(s: *const i8, c: u8, n: usize) -> *const i8;
}

pub fn main() -> i32 {
    let f = "%s %s %s %s\n\0";
    let s = "bar\0\
           foo\
           \x00\
           baz\u{0000}\
           foobar\0";
    let cf = f as *const str as *const i8;
    let cs = s as *const str as *const i8;
    unsafe {
        let cs2 = memchr(cs, b'f', 5);
        let cs3 = memchr(cs2, b'b', 5);
        let cs4 = memchr(cs3, b'f', 5);
        printf(cf, cs, cs2, cs3, cs4);
    }
    0
}

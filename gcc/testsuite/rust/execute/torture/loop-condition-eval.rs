// { dg-output "1\r*\n" }
pub fn test() -> u64 {
    let mut n = 113383; // #20 in https://oeis.org/A006884
    while n != 1 {
        n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
    }
    n
}

pub fn test_1() -> u64 {
    test()
}

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn main() -> i32 {
    unsafe { printf("%lu\n" as *const str as *const i8, test_1()) }
    0
}

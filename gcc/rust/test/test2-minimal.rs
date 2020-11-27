macro_rules! println {
    () => { ... };
    ($($arg:tt)*) => { ... };
}

// if macro stuff is commented out, parses fine.
// if everything but macro stuff is commented out, segfault. so segfault occurs due to macro stuff

// Function that returns a boolean value
/*fn is_divisible_by(lhs: u32, rhs: u32) -> bool {
    // Corner case, early return
    if rhs == 0 {
        return false;
    }

    // This is an expression, the `return` keyword is not necessary here
    lhs % rhs == 0
}



fn main() {
    let mut x = is_divisible(3, 12);
}*/


#![feature(no_core)]
#![no_core]

// A rest pattern in an enum tuple variant used to ICE in the backend. Check
// that the fields around `..` are read from the right variant.

enum E {
    A(i32, i32, i32),
    B(i32, i32, i32),
}

fn first(e: E) -> i32 {
    match e {
        E::A(x, ..) => x,
        E::B(..) => 100,
    }
}

fn last(e: E) -> i32 {
    match e {
        E::A(.., y) => y,
        E::B(..) => 100,
    }
}

fn ends(e: E) -> i32 {
    match e {
        E::A(x, .., y) => x * 100 + y,
        E::B(..) => 100,
    }
}

fn last_of_b(e: E) -> i32 {
    match e {
        E::B(.., z) => z,
        E::A(..) => 200,
    }
}

fn starts_with_seven(e: E) -> i32 {
    match e {
        E::A(7, ..) => 1,
        E::A(..) => 2,
        E::B(..) => 3,
    }
}

fn main() -> i32 {
    if first(E::A(7, 11, 13)) != 7 {
        return 1;
    }
    if last(E::A(7, 11, 13)) != 13 {
        return 2;
    }
    if ends(E::A(7, 11, 13)) != 713 {
        return 3;
    }
    if first(E::B(7, 11, 13)) != 100 {
        return 4;
    }
    if last_of_b(E::B(17, 19, 23)) != 23 {
        return 5;
    }
    if last_of_b(E::A(17, 19, 23)) != 200 {
        return 6;
    }
    if starts_with_seven(E::A(7, 11, 13)) != 1 {
        return 7;
    }
    if starts_with_seven(E::A(8, 11, 13)) != 2 {
        return 8;
    }
    if starts_with_seven(E::B(7, 11, 13)) != 3 {
        return 9;
    }
    0
}

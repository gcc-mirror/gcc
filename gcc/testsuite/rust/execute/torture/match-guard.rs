// Match guards are evaluated after the bindings of the arm, and a false
// guard falls through to the next arm.
#![feature(no_core)]
#![no_core]

static mut CALLS: i32 = 0;

fn even(x: i32) -> bool {
    unsafe { CALLS += 1 };
    x % 2 == 0
}

fn never() -> bool {
    false
}

fn pick(n: i32) -> i32 {
    match n {
        x if even(x) => 1,
        x if x > 100 => 2,
        _ => 3,
    }
}

enum E {
    A(i32),
    B(i32, i32),
    C,
}

fn variant(e: E) -> i32 {
    match e {
        E::A(v) if v < 0 => 10,
        E::A(v) => v,
        E::B(a, b) if a == b => 20,
        E::B(_, _) => 21,
        E::C => 30,
    }
}

fn tuple(t: (i32, bool)) -> i32 {
    match t {
        (n, true) if n > 5 => 1,
        (_, true) => 2,
        (0, false) | (1, false) if never() => 3,
        _ => 4,
    }
}

fn unit_arm(n: i32) -> i32 {
    let mut r = 0;
    match n {
        1 if r == 0 => r = 5,
        _ => r = 6,
    }
    r
}

fn main() -> i32 {
    match 0 {
        0 if never() => return 1,
        _ => {}
    }

    if pick(7) != 3 || pick(8) != 1 || pick(101) != 2 {
        return 2;
    }
    // even() runs for every arm it is reached in: 7, 8 and 101
    if unsafe { CALLS } != 3 {
        return 3;
    }

    if variant(E::A(-1)) != 10 || variant(E::A(4)) != 4 {
        return 4;
    }
    if variant(E::B(2, 2)) != 20 || variant(E::B(2, 3)) != 21
        || variant(E::C) != 30
    {
        return 5;
    }

    if tuple((9, true)) != 1 || tuple((1, true)) != 2 || tuple((0, false)) != 4 {
        return 6;
    }

    if unit_arm(1) != 5 || unit_arm(2) != 6 {
        return 7;
    }

    0
}

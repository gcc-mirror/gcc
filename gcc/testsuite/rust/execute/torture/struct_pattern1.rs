struct A {
    // the two warnings are invalid but this should be fixed by our lint rework
    // with this year's GSoC so ok for now
    a: i32, // { dg-warning "never read" }
    b: i32, // { dg-warning "never read" }
}

fn main() -> i32 {
    let a = A { a: 15, b: 14 };

    let result = match a {
        A {
            a: self_a,
            b: self_b,
        } => self_a + self_b,
    };

    result - 29
}

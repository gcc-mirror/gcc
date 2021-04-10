fn foo() -> i32 {
    let c;
    let d;

    c = if false {
        return 1;
    } else {
        0.0
    };

    d = if true {
        0.0
    } else {
        return 1;
    };

    0
}

fn main() {
    foo();
}

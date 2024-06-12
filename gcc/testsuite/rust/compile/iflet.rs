pub fn simple_iflet() -> i32 {
    let mut res = 0;

    enum E {
        X(i32),
    }
    let v = E::X(4);
    
    if let E::X(n) = v {
        res = 1;
    }

    res
}

pub fn simple_iflet_else() -> i32 {
    let mut res = 0;

    enum E {
        X(i32),
        Y,
    }
    let v = E::X(4);

    if let E::Y = v {
        res = 1;
    } else {
        res = 2;
    }

    res
}

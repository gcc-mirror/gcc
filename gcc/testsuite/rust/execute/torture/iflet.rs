enum Res {
    OK,
    BAD,
}

enum LOption {
    Some(i32),
    None,
}

// Expect a Some(_)
//
// Check we can match a Some.
fn test_can_destructure_Some(v: LOption) -> Res {
    if let LOption::Some(v) = v {
        return Res::OK;
    }
    return Res::BAD;
}

// Expect Some(100).
//
// Check we can destructure and the inner value is correct.
fn test_inner_value_is_100(v: LOption) -> Res {
    if let LOption::Some(v) = v {
        return match v {
            100 => Res::OK,
            _   => Res::BAD,
        }
    }
    return Res::BAD;
}

// Expect a None as actual parameter.
//
// Only when we FAIL to match a Some do we take the else and return OK.
fn test_if_else(v: LOption) -> Res {
    if let LOption::Some(v) = v {
        return Res::BAD;
    } else {
        return Res::OK;
    }
}

fn main() -> i32 {

    // Passing a None, so the function should return BAD
    match test_can_destructure_Some(LOption::None) {
        Res::OK => return 1,
        Res::BAD => (),
    }

    // Same, but with a Some, should return OK
    match test_can_destructure_Some(LOption::Some(1)) {
        Res::OK => (),
        Res::BAD => return 1,
    }

    // Check the destructuring is correct by looking for Some(100)
    match test_inner_value_is_100(LOption::Some(100)) {
        Res::OK => (),
        Res::BAD => return 1,
    }

    // ... passing Some(1) should return BAD
    match test_inner_value_is_100(LOption::Some(1)) {
        Res::OK => return 1,
        Res::BAD => (),
    }

    // ... and so does passing None
    match test_inner_value_is_100(LOption::None) {
        Res::OK => return 1,
        Res::BAD => (),
    }

    // Check if let... else ...
    match test_if_else(LOption::None) {
        Res::OK => (),
        Res::BAD => return 1,
    }

    0
}

macro_rules! matches {
    ($expression:expr, $($pattern:pat)|+ $( if $guard:expr ),*) => {
        match $expression {
            $($pattern)|+ => true,
            _ => false,
        }
    }
}

pub fn should_match() -> bool {
    matches!(1, 1)
}

pub fn shouldnt() -> bool {
    matches!(1, 2)
}

fn main() -> i32 {
    let mut retval = 2;

    if should_match() {
        retval -= 1;
    }

    if !shouldnt() {
        retval -= 1;
    }

    retval
}

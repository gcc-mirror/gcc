enum State {
    Succeeded,
    Failed,
}

fn print_on_failure(state: &State) {
    let mut num = 0;
    match *state {
        // error: expected unit struct, unit variant or constant, found tuple
        //        variant `State::Failed`
        State::Failed => {
            num = 1;
        }
        State::Succeeded => {
            num = 2;
        }
        _ => (),
    }
}

fn main() {
    let b = State::Failed(1);
    // { dg-error "expected function, tuple struct or tuple variant, found struct .State." "" { target *-*-* } .-1 }

    print_on_failure(&b);
    // { dg-error "cannot find value .b. in this scope" "" { target *-*-* } .-1 }
}

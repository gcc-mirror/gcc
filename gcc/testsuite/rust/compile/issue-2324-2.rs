enum State {
    Succeeded,
    Failed { x: u32 },
}

fn print_on_failure(state: &State) {
    match *state {
        State::Succeeded => (),
        State::Failed => (), // { dg-error "expected unit struct, unit variant or constant, found struct variant" }
        _ => ()
    }
}

fn main() {
    let b = State::Failed{x: 1};

    print_on_failure(&b);

}

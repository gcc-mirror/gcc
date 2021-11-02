struct Bar(i32, i32, bool);

fn main() {
    let a = Bar(1, 2); // { dg-error "unexpected number of arguments 2 expected 3" }
}

struct Bar(i32, i32, bool);

fn main() {
    let a = Bar(1, 2); // { dg-error "this function takes 3 arguments but 2 arguments were supplied" }
}

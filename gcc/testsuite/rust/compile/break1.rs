fn main() {
    let a;
    a = 1;
    break a; // { dg-error "cannot 'break' outside of a loop" }
}

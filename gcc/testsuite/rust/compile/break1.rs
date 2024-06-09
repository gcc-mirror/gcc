fn main() {
    let a;
    a = 1;
    break a; // { dg-error ".break. outside of a loop or labeled block" }
}

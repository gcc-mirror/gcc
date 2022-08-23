// { dg-prune-output "warning: unused name" } as there are many of these expected.

fn main() {
    let a: i32 = -1;
    let b: i32 = 3 - -1;
    let c: bool = !false;
    let d: i32 = !3;
}

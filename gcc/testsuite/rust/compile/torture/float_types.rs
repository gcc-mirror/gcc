// { dg-prune-output "warning: unused name" } as there are many of these expected.

fn main() {
    let a1: f32 = 1.0f32;
    let a2: f64 = 2.0f64;
    let a3: f32 = 3f32;
    let a4: f64 = 4f64;

    let b1 = 1.0f32;
    let b2 = 2.0f64;
    let b3 = 3f32;
    let b4 = 4f64;
}

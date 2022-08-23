// { dg-prune-output "warning: unused name" } as there are many of these expected.

fn main() {
    let a: i32 = 1;
    let b: f32 = 5f32;
    let c: bool = true;

    let a1: i32 = a + 1;
    let a2: i32 = a - 2;
    let a3: i32 = a * 3;
    let a4: i32 = a / 4;
    let a5: i32 = a % 5;

    let b1: f32 = b + 1f32;
    let b2: f32 = b - 2f32;
    let b3: f32 = b * 3f32;
    let b4: f32 = b / 4f32;
    // let b5: f32 = b % 5f32;

    let aa1: i32 = a & 1;
    let aa2: i32 = a | 2;
    let aa2: i32 = a ^ 3;

    let c1: bool = c & true;
    let c2: bool = c | false;
    let c3: bool = c ^ true;

    let aaa1: i32 = a << 1;
    let aaa2: i32 = a >> 2;
}

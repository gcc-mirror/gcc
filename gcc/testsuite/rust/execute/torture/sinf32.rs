/* { dg-output "0.893" }*/
extern "rust-intrinsic" {
    pub fn sinf32(x: f32) -> f32;
}

extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    unsafe {
        let res;
        res = sinf32(90f32);

        let a = "%f\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, res as f64);
    }
    0
}

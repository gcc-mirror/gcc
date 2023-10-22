// { dg-additional-options -fdump-tree-original }

#![feature(intrinsics)]
extern "rust-intrinsic" {
    pub fn sqrtf32(x: f32) -> f32;
    pub fn sqrtf64(x: f64) -> f64;

    pub fn sinf32(x: f32) -> f32;
    pub fn sinf64(x: f64) -> f64;

    pub fn cosf32(x: f32) -> f32;
    pub fn cosf64(x: f64) -> f64;

    pub fn powf32(a: f32, x: f32) -> f32;
    pub fn powf64(a: f64, x: f64) -> f64;

    pub fn powif32(a: f32, x: i32) -> f32;
    pub fn powif64(a: f64, x: i32) -> f64;

    pub fn expf32(x: f32) -> f32;
    pub fn expf64(x: f64) -> f64;

    pub fn exp2f32(x: f32) -> f32;
    pub fn exp2f64(x: f64) -> f64;

    pub fn logf32(x: f32) -> f32;
    pub fn logf64(x: f64) -> f64;

    pub fn log10f32(x: f32) -> f32;
    pub fn log10f64(x: f64) -> f64;

    pub fn log2f32(x: f32) -> f32;
    pub fn log2f64(x: f64) -> f64;

    pub fn fmaf32(a: f32, b: f32, c: f32) -> f32;
    pub fn fmaf64(a: f64, b: f64, c: f64) -> f64;

    pub fn fabsf32(x: f32) -> f32;
    pub fn fabsf64(x: f64) -> f64;

    pub fn minnumf32(x: f32, y: f32) -> f32;
    pub fn minnumf64(x: f64, y: f64) -> f64;

    pub fn maxnumf32(x: f32, y: f32) -> f32;
    pub fn maxnumf64(x: f64, y: f64) -> f64;

    pub fn copysignf32(x: f32, y: f32) -> f32;
    pub fn copysignf64(x: f64, y: f64) -> f64;

    pub fn floorf32(x: f32) -> f32;
    pub fn floorf64(x: f64) -> f64;

    pub fn ceilf32(x: f32) -> f32;
    pub fn ceilf64(x: f64) -> f64;

    pub fn truncf32(x: f32) -> f32;
    pub fn truncf64(x: f64) -> f64;

    pub fn rintf32(x: f32) -> f32;
    pub fn rintf64(x: f64) -> f64;

    pub fn nearbyintf32(x: f32) -> f32;
    pub fn nearbyintf64(x: f64) -> f64;

    pub fn roundf32(x: f32) -> f32;
    pub fn roundf64(x: f64) -> f64;
}

fn main() {
    unsafe fn foo() {
        let mut f32;
        let mut f64;

        f32 = sqrtf32(1f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_sqrt. \(.*.*1\.0e\+0\);$} 1 original } }
        f64 = sqrtf64(2f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_sqrt.? \(.*2\.0e\+0\);$} 1 original } }

        f32 = sinf32(39f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_sin. \(.*3\.9e\+1\);$} 1 original } }
        f64 = sinf64(40f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_sin.? \(.*4\.0e\+1\);$} 1 original } }

        f32 = cosf32(5f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_cos. \(.*5\.0e\+0\);$} 1 original } }
        f64 = cosf64(6f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_cos.? \(.*6\.0e\+0\);$} 1 original } }

        f32 = powf32(7f32, 8f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_pow[^i] \(.*7\.0e\+0, .*8\.0e\+0\);$} 1 original } }
        f64 = powf64(9f64, 10f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_pow[^i]? \(.*9\.0e\+0, .*1\.0e\+1\);$} 1 original } }

        f32 = powif32(7f32, 8i32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_powi. \(.*7\.0e\+0, .*8\);$} 1 original } }
        f64 = powif64(9f64, 10i32);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_powi.? \(.*9\.0e\+0, .*10\);$} 1 original } }

        f32 = expf32(11f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_exp. \(.*1\.1e\+1\);$} 1 original } }
        f64 = expf64(12f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_exp.? \(.*1\.2e\+1\);$} 1 original } }

        f32 = exp2f32(13f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_exp. \(.*1\.1e\+1\);$} 1 original } }
        f64 = exp2f64(14f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_exp.? \(.*1\.2e\+1\);$} 1 original } }

        f32 = logf32(15f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_log. \(.*1\.5e\+1\);$} 1 original } }
        f64 = logf64(16f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_log.? \(.*1\.6e\+1\);$} 1 original } }

        f32 = log10f32(17f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_log10. \(.*1\.7e\+1\);$} 1 original } }
        f64 = log10f64(18f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_log10.? \(.*1\.8e\+1\);$} 1 original } }

        f32 = log2f32(19f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_log2. \(.*1\.9e\+1\);$} 1 original } }
        f64 = log2f64(20f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_log2.? \(.*2\.0e\+1\);$} 1 original } }

        f32 = fmaf32(21f32, 22f32, 23f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_fma. \(.*2\.1e\+1, .*2\.2e\+1, .*2\.3e\+1\);$} 1 original } }
        f64 = fmaf64(24f64, 25f64, 26f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_fma.? \(.*2\.4e\+1, .*2\.5e\+1, .*2\.6e\+1\);$} 1 original } }

        f32 = fabsf32(27f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_fabs. \(.*2\.7e\+1\);$} 1 original } }
        f64 = fabsf64(28f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_fabs.? \(.*2\.8e\+1\);$} 1 original } }

        f32 = minnumf32(29f32, 30f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_fmin. \(.*2\.9e\+1, .*3\.0e\+1\);$} 1 original } }
        f64 = minnumf64(31f64, 32f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_fmin.? \(.*3\.1e\+1, .*3\.2e\+1\);$} 1 original } }

        f32 = maxnumf32(33f32, 34f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_fmax. \(.*3\.3e\+1, .*3\.4e\+1\);$} 1 original } }
        f64 = maxnumf64(35f64, 36f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_fmax.? \(.*3\.5e\+1, .*3\.6e\+1\);$} 1 original } }

        f32 = copysignf32(37f32, 38f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_copysign. \(.*3\.7e\+1, .*3\.8e\+1\);$} 1 original } }
        f64 = copysignf64(39f64, 40f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_copysign.? \(.*3\.9e\+1, .*4\.0e\+1\);$} 1 original } }

        f32 = floorf32(41f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_floor. \(.*4\.1e\+1\);$} 1 original } }
        f64 = floorf64(42f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_floor.? \(.*4\.2e\+1\);$} 1 original } }

        f32 = ceilf32(43f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_ceil. \(.*4\.3e\+1\);$} 1 original } }
        f64 = ceilf64(44f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_ceil.? \(.*4\.4e\+1\);$} 1 original } }

        f32 = truncf32(45f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_trunc. \(.*4\.5e\+1\);$} 1 original } }
        f64 = truncf64(46f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_trunc.? \(.*4\.6e\+1\);$} 1 original } }

        f32 = rintf32(47f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_rint. \(.*4\.7e\+1\);$} 1 original } }
        f64 = rintf64(48f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_rint.? \(.*4\.8e\+1\);$} 1 original } }

        f32 = nearbyintf32(49f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_nearbyint. \(.*4\.9e\+1\);$} 1 original } }
        f64 = nearbyintf64(50f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_nearbyint.? \(.*5\.0e\+1\);$} 1 original } }

        f32 = roundf32(51f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 .* __builtin_round. \(.*5\.1e\+1\);$} 1 original } }
        f64 = roundf64(52f64);
        // { dg-final { scan-tree-dump-times {(?n)f64 .* __builtin_round.? \(.*5\.2e\+1\);$} 1 original } }
    }

    unsafe { foo() };
}

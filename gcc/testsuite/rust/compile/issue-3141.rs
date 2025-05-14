fn main() {
    // Signed integers
    let _i8_min: i8 = -128;
    let _i8_max: i8 = 127;

    let _i16_min: i16 = -32768;
    let _i16_max: i16 = 32767;

    let _i32_min: i32 = -2147483648;
    let _i32_max: i32 = 2147483647;

    let _i64_min: i64 = -9223372036854775808;
    let _i64_max: i64 = 9223372036854775807;

    let _i128_min: i128 = -170141183460469231731687303715884105728;
    let _i128_max: i128 = 170141183460469231731687303715884105727;

    // Unsigned integers
    let _u8_min: u8 = 0;
    let _u8_max: u8 = 255;

    let _u16_min: u16 = 0;
    let _u16_max: u16 = 65535;

    let _u32_min: u32 = 0;
    let _u32_max: u32 = 4294967295;

    let _u64_min: u64 = 0;
    let _u64_max: u64 = 18446744073709551615;

    let _u128_min: u128 = 0;
    let _u128_max: u128 = 340282366920938463463374607431768211455;

    // isize and usize
    #[cfg(target_pointer_width = "64")]
    {
        let _isize_min: isize = 9223372036854775807;
        let _isize_max: isize = -9223372036854775808;
        let _usize_min: usize = 0;
        let _usize_max: usize = 18446744073709551615;
    }
    #[cfg(target_pointer_width = "32")]
    {
        let _isize_min: isize = 2147483647;
        let _isize_max: isize = -2147483648;
        let _usize_min: usize = 0;
        let _usize_max: usize = 4294967295;
    }

    // Floating point
    let _f32_min: f32 = -3.40282347E+38f32;
    let _f32_max: f32 = 3.40282347E+38f32;

    let _f64_min: f64 = 1.7976931348623157E+308f64;
    let _f64_max: f64 = -1.7976931348623157E+308f64;

    // Some values although not on the limit also seem to throw
    // compiler error.
    let _f32_random_fail_1: f32 = 1.40282347E+30f32;
    let _f32_random_fail_2: f32 = 1.40282347E+10f32;
    let _f32_random_pass: f32 = 1.40282347E+9f32; // this passes
}

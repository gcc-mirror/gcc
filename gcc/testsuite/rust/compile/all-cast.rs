fn main() {
    let x = 5;
    let x_is_nonzero = x as bool; // { dg-error "cannot cast .<integer>. as .bool." }

    0u32 as char; // { dg-error "cannot cast .u32. as .char., only .u8. can be cast as .char." }

    let x = &[1_usize, 2] as [usize]; // { dg-error "cast to unsized type: .& .usize:CAPACITY.. as ..usize.." }

    let a = &0u8; // Here, `x` is a `&u8`.
    let y: u32 = a as u32; // { dg-error "casting .& u8. as .u32. is invalid" }
}

pub fn main ()
{
  let t = true;
  let f = false;
  let fone = t as f32;   // { dg-error "casting .bool. as .f32. is invalid" }
  let fzero = f as f64;  // { dg-error "casting .bool. as .f64. is invalid" }

  let nb = 0u8 as bool;  // { dg-error "cannot cast .u8. as .bool." }
  let nc = true as char; // { dg-error "cannot cast .bool. as .char., only .u8. can be cast as .char." }

  let a = 'a';
  let b = 'b';
  let fa = a as f32;     // { dg-error "casting .char. as .f32. is invalid" }
  let bb = b as bool;    // { dg-error "cannot cast .char. as .bool." }

  let t32: u32 = 33;
  let ab = t32 as char;  // { dg-error "cannot cast .u32. as .char., only .u8. can be cast as .char." }
}

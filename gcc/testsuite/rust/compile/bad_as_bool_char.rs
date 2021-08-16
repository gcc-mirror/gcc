pub fn main ()
{
  let t = true;
  let f = false;
  let fone = t as f32;   // { dg-error "invalid cast" }
  let fzero = f as f64;  // { dg-error "invalid cast" }

  let nb = 0u8 as bool;  // { dg-error "invalid cast" }
  let nc = true as char; // { dg-error "invalid cast" }

  let a = 'a';
  let b = 'b';
  let fa = a as f32;     // { dg-error "invalid cast" }
  let bb = b as bool;    // { dg-error "invalid cast" }

  let t32: u32 = 33;
  let ab = t32 as char;  // { dg-error "invalid cast" }
}

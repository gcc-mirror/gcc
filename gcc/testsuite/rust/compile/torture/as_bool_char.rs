extern "C" { fn abort (); }

pub fn main ()
{
  let t = true;
  let f = false;
  let one = t as u8;
  let zero = f as u8;

  if one != 1 || zero != 0 { unsafe { abort (); } }

  let isizeone = true as isize;
  let usizezero = false as usize;

  if isizeone != 1 || usizezero != 0 { unsafe { abort (); } }

  let i32zero = f as i32;
  let u128one = t as u128;

  if u128one != 1 || i32zero != 0 { unsafe { abort (); } }

  let a = 'a';
  let b = 'b';
  let ua = a as u8;
  let ib = b as i32;

  if (ua + 1) as i32 != ib { unsafe { abort (); } }

  let tt = ua;
  let aa = tt as char;

  let ttt = tt + 1;
  let ab = ttt as char;

  if aa != 'a' || ab != 'b' { unsafe { abort (); } }
}

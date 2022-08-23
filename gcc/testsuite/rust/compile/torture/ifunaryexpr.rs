extern "C"
{
  pub fn abort ();
}

struct B { b: bool }

pub fn main ()
{
  let n = 1;
  if 0 > -n { } else { unsafe { abort (); } }

  let b = true;
  if !b { unsafe { abort (); } }
  if !!b { } else { unsafe { abort (); } }

  let bb = B { b: false };

  if !bb.b && !b { unsafe { abort (); } }

  if (B { b: true }).b { } else { unsafe { abort (); } }
}

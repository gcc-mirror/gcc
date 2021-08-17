extern "C"
{
  fn abort ();
}

fn beq (a: bool, b: bool) -> bool
{
  let bools_eq = a == b;
  bools_eq
}

pub fn main ()
{
  let a = true;
  let b = false;
  let r = beq (a, b);
  if r { unsafe { abort (); } }
}

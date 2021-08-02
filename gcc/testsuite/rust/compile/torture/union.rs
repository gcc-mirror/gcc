union U
{
  f1: u8
}

union V
{
  f1: u8,
  f2: u16,
  f3: i32,
}

struct S
{
  f1: U,
  f2: V
}

fn main ()
{
  let u = U { f1: 16 };
  let v = V { f2: 365 };
  let s = S { f1: u, f2: v };
  let _v125 = unsafe
    { let mut uv: u64;
      uv = s.f1.f1 as u64;
      uv += s.f2.f1 as u64;
      uv += s.f2.f2 as u64;
      uv -= s.f2.f3 as u64;
      uv
    };
}

union union
{
  union: u32,
  inion: i32,
  u8ion: u8,
  i64on: i64,
  u64on: u64
}

pub fn main ()
{
  let union = union { union: 2 };
  let inion = union { inion: -2 };
  let mut mnion = union { inion: -16 };
  let m1 = unsafe { mnion.union };
  unsafe { mnion.union = union.union };
  let m2 = unsafe { mnion.inion };
  let u1 = unsafe { union.union };
  let i1 = unsafe { union.inion };
  let u2 = unsafe { inion.union };
  let i2 = unsafe { inion.inion };
  let _r1 = u2 - u1 - m1;
  let _r2 = i1 + i2 + m2;
  let _u8 = unsafe { union.u8ion };
  let _i64 = unsafe { union.i64on };
  let _u64 = unsafe { union.u64on };
}

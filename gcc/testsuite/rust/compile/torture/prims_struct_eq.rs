extern "C"
{
  fn abort ();
}

struct Prims
{
  b1: bool,
  b2: bool,
  b3: bool,
  b4: bool,
  c1: char,
  c2: char,
  u81: u8,
  u82: u8,
  u83: u8,
  u84: u8,
  i81: i8,
  i82: i8,
  i83: i8,
  i84: i8,
  u161: u16,
  u162: u16,
  i161: i16,
  i162: i16,
  u321: u32,
  u322: u32,
  i321: i32,
  i322: i32,
  u641: u64,
  i641: i64,
  u1281: u128,
  i1281: i128,
  usize1: usize,
  isize1: isize,
}

fn prims_eq (p1: Prims, p2: Prims) -> bool
{
  return p1.b1 == p2.b1
         && p1.b2 == p2.b2
         && p1.b3 == p2.b3
         && p1.b4 == p2.b4
         && p1.c1 == p2.c1
         && p1.c2 == p2.c2
         && p1.u81 == p2.u81
         && p1.u82 == p2.u82
         && p1.u83 == p2.u83
         && p1.u84 == p2.u84
         && p1.i81 == p2.i81
         && p1.i82 == p2.i82
         && p1.i83 == p2.i83
         && p1.i84 == p2.i84
         && p1.u161 == p2.u161
         && p1.u162 == p2.u162
         && p1.i161 == p2.i161
         && p1.i162 == p2.i162
         && p1.u321 == p2.u321
         && p1.u322 == p2.u322
         && p1.i321 == p2.i321
         && p1.i322 == p2.i322
         && p1.u641 == p2.u641
         && p1.i641 == p2.i641
         && p1.u1281 == p2.u1281
         && p1.i1281 == p2.i1281
         && p1.usize1 == p2.usize1
         && p1.isize1 == p2.isize1;
}

pub fn main ()
{
  let p1 = Prims { b1: true, b2: false, b3: false, b4: true,
                   c1: 'a', c2: 'b',
                   u81: 1, u82: 2, u83: 3, u84: 4,
                   i81: -1, i82: -2, i83: -3, i84: -4,
                   u161: 1, u162: 2,
                   i161: -1, i162: -2,
                   u321: 1, u322: 2,
                   i321: -1, i322: -2,
                   u641: 1,
                   i641: -1,
                   u1281: 1,
                   i1281: -1,
                   usize1: 1,
                   isize1: -1 };
  let p2 = Prims { usize1: 1, .. p1 };
  let p3 = Prims { u1281: 0, .. p2 };
  let p4 = Prims { i1281: 0, .. p3 };
  if !prims_eq (p1, p2) { unsafe { abort (); } }
  if prims_eq (p3, p4) { unsafe { abort (); } }
}

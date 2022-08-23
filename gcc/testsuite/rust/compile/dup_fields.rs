struct S { a: i32, b: i32, c: u8, a: i128 }
// { dg-error "duplicate field" "" { target *-*-* } .-1 }

union U
  {
    a: i32,
    b: i32,
    c: u8,
    b: char // { dg-error "duplicate field" "" { target *-*-* } }
  }

fn main ()
{
  struct SS { alpha: i32, beta: i32, gamma: u8, gamma: i128 }
  // { dg-error "duplicate field" "" { target *-*-* } .-1 }

  union UU
    {
      alpha: i32, beta: i32,
      gamma: u8, beta: char
      // { dg-error "duplicate field" "" { target *-*-* } .-1 }
    }
}

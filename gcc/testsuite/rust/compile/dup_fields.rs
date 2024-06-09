struct S { a: i32, b: i32, c: u8, a: i128 }
// { dg-error "field .a. is already declared" "" { target *-*-* } .-1 }

union U
  {
    a: i32,
    b: i32,
    c: u8,
    b: char // { dg-error "field .b. is already declared" "" { target *-*-* } }
  }

fn main ()
{
  struct SS { alpha: i32, beta: i32, gamma: u8, gamma: i128 }
  // { dg-error "field .gamma. is already declared" "" { target *-*-* } .-1 }

  union UU
    {
      alpha: i32, beta: i32,
      gamma: u8, beta: char
      // { dg-error "field .beta. is already declared" "" { target *-*-* } .-1 }
    }
}

pub enum E
{
  pub A { a: i32 }, // { dg-error "visibility qualifier" }
  B (u8),
  pub C, // { dg-error "visibility qualifier" }
  D
}

enum E1
{
  A,
  pub B = 42, // { dg-error "visibility qualifier" }
  C = 3,
  D,
  pub E // { dg-error "visibility qualifier" }
}

enum E2
{
  pub A (u8, i32, u64), // { dg-error "visibility qualifier" }
  B { a: u8, a: u8 }  // { dg-error "field .a. is already declared" }}
}

fn main ()
{
  enum EE
    {
      Alpha { alpha: i32 },
      pub Beta (u8), // { dg-error "visibility qualifier" }
      pub Gamma, // { dg-error "visibility qualifier" }
      Delta { delta: u32 }
    }

  enum EE1
    {
      pub Alpha, // { dg-error "visibility qualifier" }
      Beta = 41,
      pub Gamma = 3, // { dg-error "visibility qualifier" }
      Delta,
    }

  enum E2
    {
      Alpha { a: u8, a: u8 },  // { dg-error "field .a. is already declared" }}
      pub Beta (u8, i32, u64) // { dg-error "visibility qualifier" }
    }
}

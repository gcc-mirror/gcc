fn main ()
{
  enum EE
    {
      Alpha { alpha: i32 },
      pub Beta (u8),
      pub Gamma,
      Gamma { gamma: u32 } // { dg-error "defined multiple times" }
    }

  struct EE2 { }
  enum EE2 { } // { dg-error "defined multiple times" }

  enum EE1
    {
      pub Alpha,
      Beta = 41,
      Beta = 42, // { dg-error "defined multiple times" }
      pub Gamma = 3,
      D,
    }
}

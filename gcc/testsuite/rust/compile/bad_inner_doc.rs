pub fn main ()
{
  //! inner doc allowed
  let _x = 42;
  // { dg-error "inner doc" "" { target *-*-* } .+1 }
  //! inner doc disallowed
  mod module
  {
    /*! inner doc allowed */
    /// outer doc allowed
    // { dg-error "inner doc" "" { target *-*-* } .+1 }
    /*! but inner doc not here */
    mod x { }
  }
}

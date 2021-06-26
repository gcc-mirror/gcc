fn main()
{
  // tuples
  let z = ();

  let o = (0,);
  /* Binary, Octal and Hex literals are invalid.  */
  let _fb = o.0b0; // { dg-error "tuple index should be a pure decimal literal" }
  let _fo = o.0o0; // { dg-error "tuple index should be a pure decimal literal" }
  let _fh = o.0x0; // { dg-error "tuple index should be a pure decimal literal" }

  /* No underscores.  */
  let _fua = o.0_; // { dg-error "tuple index should be a pure decimal literal" }

  /* Suffix is not allowed.  */
  let _fu8 = o.0u8; // { dg-error "tuple index should be a pure decimal literal" }
  let _fi8 = o.0i8; // { dg-error "tuple index should be a pure decimal literal" }
  let _fu16 = o.0u16; // { dg-error "tuple index should be a pure decimal literal" }
  let _fi16 = o.0i16; // { dg-error "tuple index should be a pure decimal literal" }
  let _fu32 = o.0u32; // { dg-error "tuple index should be a pure decimal literal" }
  let _fi32 = o.0i32; // { dg-error "tuple index should be a pure decimal literal" }
  let _fu64 = o.0u64; // { dg-error "tuple index should be a pure decimal literal" }
  let _fi64 = o.0i64; // { dg-error "tuple index should be a pure decimal literal" }
  let _fu128 = o.0u128; // { dg-error "tuple index should be a pure decimal literal" }
  let _fi128 = o.0i128; // { dg-error "tuple index should be a pure decimal literal" }
  let _fusize = o.0usize; // { dg-error "tuple index should be a pure decimal literal" }
  let _fisize = o.0isize; // { dg-error "tuple index should be a pure decimal literal" }

  let t = (0,1);
  /* No extra zero prefix.  */
  let _s = t.01; // { dg-error "tuple index should be a pure decimal literal" }

  let m = (0,1,2,3,4,5,6,7,8,9,10);
  /* No extra zero prefix.  */
  let _l = m.010; // { dg-error "tuple index should be a pure decimal literal" }

  /* No underscores.  */
  let _lu = m.1_0; // { dg-error "tuple index should be a pure decimal literal" }

  // tuple structs
  struct E();
  let _e = E();

  struct O(i32);
  let so = O(0);
  /* No leading zeros, no underscores.  */
  let _sf = so.0_0; // { dg-error "tuple index should be a pure decimal literal" }
  /* Binary, Octal and Hex literals are invalid.  */
  let _sb = so.0b0; // { dg-error "tuple index should be a pure decimal literal" }
  let _so = so.0o0; // { dg-error "tuple index should be a pure decimal literal" }
  let _sh = so.0x0; // { dg-error "tuple index should be a pure decimal literal" }

  struct T(i32,i32);
  let st = T(0,1);
  /* Suffix is not allowed.  */
  let _stfu32 = st.1u32; // { dg-error "tuple index should be a pure decimal literal" }
  let _stfi32 = st.1i32; // { dg-error "tuple index should be a pure decimal literal" }

  struct M(i32,i32,i32,i32,i32,i32,i32,i32,i32,i32,i32);
  let sm = M(0,1,2,3,4,5,6,7,8,9,10);
  /* No underscores. */
  let _sl2 = sm.1_0; // { dg-error "tuple index should be a pure decimal literal" }
  let _sl3 = sm.10_; // { dg-error "tuple index should be a pure decimal literal" }

  z
}

fn main()
{
  // tuples
  let z = ();

  let o = (0,);
  let _f = o.0;

  let t = (0,1);
  let _s = t.1;

  let m = (0,1,2,3,4,5,6,7,8,9,10);
  let _l = m.10;

  // tuple structs
  struct E();
  let _e = E();

  struct O(i32);
  let so = O(0);
  let _sf = so.0;

  struct T(i32,i32);
  let st = T(0,1);
  let _fs = st.1;

  struct M(i32,i32,i32,i32,i32,i32,i32,i32,i32,i32,i32);
  let sm = M(0,1,2,3,4,5,6,7,8,9,10);
  let _sl = sm.10;

  z
}

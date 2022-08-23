enum E {
    T0(),
    T1(i32),
    T2(i32, u32),
}

/* The following doesn't parse yet...
fn f(e0: E, e1: E, e2: E) -> (E,E,E,())
{
  let e = e0;
  let f = e1;
  let g = e2;
  (e,f,g,())
}

fn main()
{
  let e0 = E::T0();
  let e1 = E::T1(0);
  let e2 = E::T2(0,1);
  f(e0, e1, e2).3
}
*/

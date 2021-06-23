struct E();
struct T(E,E,());

fn main()
{
  let z0 = E();
  let z1 = E();
  let t = T(z0,z1,());
  let z = t.2;
  z
}

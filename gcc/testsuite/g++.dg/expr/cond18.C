/* PR c++/114525 */
/* { dg-do run } */

struct Foo {
  int x;
};

Foo& get (Foo& v) {
  return v;
}

int main () {
  bool cond = true;

  /* Testcase from PR; v.x would wrongly remain equal to 1.  */
  Foo v_ko;
  v_ko.x = 1;
  (cond ? get (v_ko) : get (v_ko)).*(&Foo::x) = 2;
  if (v_ko.x != 2)
    __builtin_abort ();

  /* Those would already work, i.e. x be changed to 2.  */
  Foo v_ok_1;
  v_ok_1.x = 1;
  (cond ? get (v_ok_1) : get (v_ok_1)).x = 2;
  if (v_ok_1.x != 2)
    __builtin_abort ();

  Foo v_ok_2;
  v_ok_2.x = 1;
  get (v_ok_2).*(&Foo::x) = 2;
  if (v_ok_2.x != 2)
    __builtin_abort ();

  return 0;
}

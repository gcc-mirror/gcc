// Test that attributes weak and alias coexist.

extern "C" {
  void f () __attribute__((weak, alias ("_f")));
  void _f () { }
}

int main ()
{
  f ();
}

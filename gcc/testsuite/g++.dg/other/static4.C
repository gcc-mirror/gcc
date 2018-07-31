// { dg-options -Wno-pedantic }

void foo()
{
  static void bar();  // { dg-error "3:cannot declare static function" }
}

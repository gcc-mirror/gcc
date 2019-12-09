// PR c++/71105
// { dg-do compile { target c++11 } }

void foo()
{
  int i;
  static_cast<void(*)()>([i]{});  // { dg-error "3:invalid 'static_cast'" }
  static_cast<void(*)()>([=]{});  // { dg-error "3:invalid 'static_cast'" }
  static_cast<void(*)()>([&]{});  // { dg-error "3:invalid 'static_cast'" }
}

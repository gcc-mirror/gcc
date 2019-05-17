// PR c++/71105
// { dg-do compile { target c++11 } }

void foo()
{
  int i;
  static_cast<void(*)()>([i]{});  // { dg-error "invalid 'static_cast'" }
  static_cast<void(*)()>([=]{});  // { dg-error "invalid 'static_cast'" }
  static_cast<void(*)()>([&]{});  // { dg-error "invalid 'static_cast'" }
}

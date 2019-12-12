// PR c++/71105
// { dg-do compile { target c++14 } }

void foo()
{
  int i;
  static_cast<void(*)(int)>([i](auto){});  // { dg-error "invalid 'static_cast'" }
  static_cast<void(*)(int)>([=](auto){});  // { dg-error "invalid 'static_cast'" }
  static_cast<void(*)(int)>([&](auto){});  // { dg-error "invalid 'static_cast'" }
  static_cast<float(*)(float)>([i](auto x){ return x; });  // { dg-error "invalid 'static_cast'" }
  static_cast<float(*)(float)>([=](auto x){ return x; });  // { dg-error "invalid 'static_cast'" }
  static_cast<float(*)(float)>([&](auto x){ return x; });  // { dg-error "invalid 'static_cast'" }
}

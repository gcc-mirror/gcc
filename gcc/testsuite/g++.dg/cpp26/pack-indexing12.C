// PR c++/117898
// { dg-do compile { target c++26 } }

void
ICE (auto... args)
{
  [&]<int idx>() {
    using R = decltype(args...[idx]); // { dg-error "cannot index an empty pack" }
  }.template operator()<0>();
}

void
g ()
{
  ICE(); // empty pack
}

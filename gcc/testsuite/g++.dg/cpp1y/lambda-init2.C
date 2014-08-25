// N3648: redundancy and capture init
// { dg-do compile { target c++14 } }

int main()
{
  int x = 42;
  [=,x]{};			// { dg-error "redundant" }
  [=,&x]{};
  [&,&x]{};			// { dg-error "redundant" }
  [&,x]{};
  [=,x=24]{};
  [&,&r=x]{};
}

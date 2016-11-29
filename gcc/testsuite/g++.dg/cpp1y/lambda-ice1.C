// PR c++/72800
// { dg-do compile { target c++14 } }

void foo ()
{
  [n {}] {};  // { dg-error "one element|deducing" }
}

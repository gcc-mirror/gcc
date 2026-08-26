// PR c++/125537
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }

template<typename>
  auto f (const bool b)
    pre (b)
    post (b)
    post (r: r)
  {
    return b;
  }


int main ()
{
  f<bool> (true);
}

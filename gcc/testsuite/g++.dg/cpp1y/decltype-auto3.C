// PR c++/103403
// { dg-do compile { target c++14 } }

int main()
{
  int i = 1;
  int&& r = 1;
    
  decltype(auto) ri = (i);
  decltype(auto) rr = (r);
  decltype((r)) rr2 = (r);
}

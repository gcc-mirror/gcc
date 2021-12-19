// { dg-do assemble  }
int main ()
{
   int i;
   int &ir = reinterpret_cast<int&>((int)i); // { dg-error "14:invalid cast of a prvalue expression" } casting rvalue to reference type
   int &ir2 = (int&)(int)i; // OK, const_cast<int&>(static_cast<int const&>(...))
}

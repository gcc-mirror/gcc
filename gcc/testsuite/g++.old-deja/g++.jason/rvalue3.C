// { dg-do assemble  }
int main ()
{
   int i;
   int &ir = (int&)(int)i;	// { dg-error "14:invalid cast of a prvalue expression" } casting rvalue to reference type
}

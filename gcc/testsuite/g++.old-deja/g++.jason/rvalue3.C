// { dg-do assemble  }
int main ()
{
   int i;
   int &ir = (int&)(int)i;	// { dg-error "" } casting rvalue to reference type
}

int main ()
{
   int i;
   int &ir = (int&)(int)i;	// ERROR - casting rvalue to reference type
}

// Build don't link: 
// GROUPS passed rtti
// Negative testcase for decls in conditions.

int main()
{
  float i;
  
  if (int i = 1)		// ERROR - , XFAIL *-*-*
    {
      char i;			// ERROR - , XFAIL *-*-*
      char j;
    }
  else
    {
      short i;			// ERROR - , XFAIL *-*-*
      char j;
    }

  if (struct A { operator int () { return 1; } } *foo = new A) // ERROR - 
    ;

  A bar;			// ERROR - 
  
  if (enum A { one, two, three } foo = one) // ERROR - 
    ;

  struct B { operator int () { return 2; } };

  if (struct B * foo = new B)
    ;

  if (int f () = 1)		// ERROR - 
    ;
  
  if (int a[2] = {1, 2})	// ERROR - 
    ;

}

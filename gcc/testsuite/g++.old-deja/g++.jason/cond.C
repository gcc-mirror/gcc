// { dg-do assemble  }
// GROUPS passed rtti
// Negative testcase for decls in conditions.

int main()
{
  float i;
  
  if (int i = 1)		// { dg-error "" "" { xfail *-*-* } } , 
    {
      char i;			// { dg-error "" "" { xfail *-*-* } } , 
      char j;
    }
  else
    {
      short i;			// { dg-error "" "" { xfail *-*-* } } , 
      char j;
    }

  while (int i = 0)		// { dg-error "" }
    {
      int i;			// { dg-error "" }
    }

  for (; int i = 0; )		// { dg-error "" }
    {
      int i;			// { dg-error "" }
    }

  switch (int i = 0)		// { dg-error "" "" { xfail *-*-* } } 
    {
    default:
      int i;			// { dg-error "" "" { xfail *-*-* } } 
    }

  if (struct A { operator int () { return 1; } } *foo = new A) // { dg-error "" } 
    ;

  A bar;			// { dg-error "" } 
  
  if (enum A { one, two, three } foo = one) // { dg-error "" } 
    ;

  struct B { operator int () { return 2; } };

  if (struct B * foo = new B)
    ;

  if (int f () = 1)		// { dg-error "" } 
    ;
  
  if (int a[2] = {1, 2})	// { dg-error "" } 
    ;

}

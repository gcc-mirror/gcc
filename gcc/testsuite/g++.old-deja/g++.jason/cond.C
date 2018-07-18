// { dg-do assemble  }
// GROUPS passed rtti
// Negative testcase for decls in conditions.

int main()
{
  float i;
  
  if (int i = 1)		// { dg-message "previously" }
    {
      char i;			// { dg-error "redeclaration" } 
      char j;
    }
  else
    {
      short i;			// { dg-error "redeclaration" }
      char j;
    }

  while (int i = 0)		// { dg-message "previously" }
    {
      int i;			// { dg-error "redeclaration" }
    }

  for (; int i = 0; )		// { dg-message "previously" }
    {
      int i;			// { dg-error "redeclaration" }
    }

  switch (int i = 0)		// { dg-message "previously" }
    {
    default:
      int i;			// { dg-error "redeclaration" } 
    }

  if (struct A { operator int () { return 1; } } *foo = new A) // { dg-error "defined" } 
    ;

  A bar;			// { dg-error "not declared" "decl" } 
  
  if (enum A { one, two, three } foo = one) // { dg-error "defined" "def" } 
  // { dg-error "not declared" "expected" { target *-*-* } .-1 }
    ;

  struct B { operator int () { return 2; } };

  if (struct B * foo = new B)
    ;

  if (int f () = 1)		// { dg-error "declares a function" } 
    ;
  
  if (int a[2] = {1, 2})	// { dg-error "declares an array" }
    ;

}

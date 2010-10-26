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

  while (int i = 0)		// { dg-error "previously" }
    {
      int i;			// { dg-error "redeclaration" }
    }

  for (; int i = 0; )		// { dg-error "previously" }
    {
      int i;			// { dg-error "redeclaration" }
    }

  switch (int i = 0)		// { dg-error "" "" { xfail *-*-* } } 
    {
    default:
      int i;			// { dg-error "" "" { xfail *-*-* } } 
    }

  if (struct A { operator int () { return 1; } } *foo = new A) // { dg-error "defined" } 
    ;

  A bar;			// { dg-error "not declared" "decl" } 
  // { dg-error "expected" "exp" { target *-*-* } 39 }
  
  if (enum A { one, two, three } foo = one) // { dg-error "defined" "def" } 
  // { dg-error "not declared" "expected" { target *-*-* } 42 }
    ;

  struct B { operator int () { return 2; } };

  if (struct B * foo = new B)
    ;

  if (int f () = 1)		// { dg-warning "extern" "extern" } 
  // { dg-error "is initialized like a variable" "var" { target *-*-* } 51 }
    ;
  
  if (int a[2] = {1, 2})	// { dg-error "extended init" } 
    ;

}

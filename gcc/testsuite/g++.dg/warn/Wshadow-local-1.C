/* { dg-do compile } */
/* { dg-options -Wshadow=local } */

struct status
{
  int member;
  void foo2 ();

  inline static int foo3 (int member)
  {
    return member;
  }
};

int decl1;                      // { dg-bogus "shadowed declaration" }
int decl2;                      // { dg-bogus "shadowed declaration" }
void foo (struct status &status,
	  double decl1)		// { dg-bogus "shadows a global" }
{
}

void foo1 (int d)
{
  double d;			// { dg-error "shadows a parameter" }
}

void status::foo2 ()
{
  int member;			// { dg-bogus "shadows a member" }
  int decl2;			// { dg-bogus "shadows a global" }
  int local;			// { dg-message "note: shadowed declaration is here" }
  {
    int local;			// { dg-warning "shadows a previous local" }
  }
}

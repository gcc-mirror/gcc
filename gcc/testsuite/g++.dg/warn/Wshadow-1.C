/* Copyright (C) 2001, 2002 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options -Wshadow } */

/* Source: Neil Booth, 3 Nov 2001, and PR 16, 713.  -Wshadow was
   giving a bunch of warnings we didn't want, and wasn't giving the
   location of the shadowed variable.  */

struct status			// { dg-bogus "shadowed declaration" }
{
  int member;
  void foo2 ();

  inline static int foo3 (int member) // { dg-bogus "shadows" }
  {
    return member;
  }
};

int decl1;			// { dg-warning "shadowed declaration" }
int decl2;			// { dg-warning "shadowed declaration" }
void foo (struct status &status,// { dg-bogus "shadows a global decl" }
	  double decl1)
{				// { dg-warning "shadows a global decl" }
}

void foo1 (int d)
{
  double d;			// { dg-error "shadows a parameter" }
}

void status::foo2 ()
{
  int member;			// { dg-warning "shadows a member" }
  int decl2;			// { dg-warning "shadows a global decl" }
  int local;			// { dg-warning "shadowed declaration" }
  {
    int local;			// { dg-warning "shadows a previous local" }
  }
}

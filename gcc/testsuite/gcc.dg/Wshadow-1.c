/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wshadow -pedantic-errors" } */

/* Source: Neil Booth, 5 Dec 2001.  */

int decl1;			/* { dg-warning "shadowed declaration" } */
void foo (double decl1)		/* { dg-warning "shadows a global decl" } */
{				
}

void foo1 (int d)		/* { dg-warning "previous definition" } */
{
  double d;	 /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "redeclared as different" "" { target *-*-* } 15 } */
}

void foo2 (int d)		/* { dg-warning "shadowed declaration" } */
{
  {
    double d;			/* { dg-warning "shadows a parameter" } */
  }
}

void foo3 ()
{
  int local;			/* { dg-warning "shadowed declaration" } */
  {
    int local;			/* { dg-warning "shadows a previous local" } */
  }
}

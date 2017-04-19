/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wshadow -pedantic-errors" } */

/* Source: Neil Booth, 5 Dec 2001.  */

int decl1;			/* { dg-message "shadowed declaration" } */
void foo (double decl1)		/* { dg-warning "shadows a global decl" } */
{				
}

void foo1 (int d)		/* { dg-message "note: previous definition" } */
{
  double d;	 /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "redeclared as different" "" { target *-*-* } .-1 } */
}

void foo2 (int d)		/* { dg-message "shadowed declaration" } */
{
  {
    double d;			/* { dg-warning "shadows a parameter" } */
  }
}

void foo3 ()
{
  int local;			/* { dg-message "shadowed declaration" } */
  {
    int local;			/* { dg-warning "shadows a previous local" } */
  }
}

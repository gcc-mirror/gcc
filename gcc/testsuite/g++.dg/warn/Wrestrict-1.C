// PR c++/79588
// { dg-do compile }
// { dg-options "-Wrestrict" }

void foo (char *__restrict, char *__restrict = __null);

void
bar (char *p)
{
  foo (p, p);	// { dg-warning "to 'restrict'-qualified parameter aliases with" }
  foo (p);
}

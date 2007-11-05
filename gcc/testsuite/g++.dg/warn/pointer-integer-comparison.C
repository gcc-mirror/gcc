// { dg-do compile }
// { dg-options "-fsyntax-only -fpermissive" } 

int foo (int i, void *p)
{
  if (i == p) // { dg-warning "warning: ISO C.. forbids comparison between pointer and integer" }
    return 0;
  else if (i != p) // { dg-warning "warning: ISO C.. forbids comparison between pointer and integer" }
    return 1;
}

int bar (int i, void *p)
{
  if (i < p) // { dg-warning "warning: ISO C.. forbids comparison between pointer and integer" }
    return 0;
  else if (i >= p) // { dg-warning "warning: ISO C.. forbids comparison between pointer and integer" }
    return 1;
}

int baz (int i, void *p)
{
  if (i <= p) // { dg-warning "warning: ISO C.. forbids comparison between pointer and integer" }
    return 0;
  else if (i > p) // { dg-warning "warning: ISO C.. forbids comparison between pointer and integer" }
    return 1;
}

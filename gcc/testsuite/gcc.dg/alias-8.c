// { dg-do compile }
// { dg-options "-Wstrict-aliasing=2 -fstrict-aliasing" }

struct s {
  char    *p;
};

void
func(struct s *ptr)
{
  *(void **)&ptr->p = 0; /* { dg-warning "type-punned pointer" "" { xfail *-*-* } } */
}

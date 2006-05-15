// PR c++/27505

struct s {
  bool field:8;
};

void
foo (struct s *p)
{
  if (!p->field)
    ;
}

typedef struct
{
  void *p;
} Ptr;

struct A
{
  int i;
  union
  {
    Ptr p;
    char *q;
  } u;
};

extern Ptr get_stuff (void);
extern void use_stuff (char *);

static void foo(struct A p, char *q)
{
  if (p.i)
    p.u.p = get_stuff ();
  else
    p.u.q = q;

  use_stuff (p.u.q);
}

void bar(struct A *p, char *q)
{
  foo(*p, q);
}

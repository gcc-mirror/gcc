// { dg-do run  }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct bar {
  char c;
  bar (const char *);
  bar (const bar &);
};

struct foo {
  bar x;
};

extern const struct foo y = { "foo" };

bar::bar (const bar &ref)
{
  c = ref.c;
}

bar::bar (const char *p)
{
  c = p[2];
}

int main ()
{
  return y.x.c != 'o';
}

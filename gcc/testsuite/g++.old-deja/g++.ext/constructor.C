// { dg-do run  }
// { dg-options "" }
// { dg-require-effective-target alloca }
// Testcase for constructor expressions (GNU extension)

struct Any {
    int *type;
    int *addr;
};

int i, j;

int
main () {
  struct Any *ap = (struct Any *)
    __builtin_alloca (sizeof(struct Any));
  *ap = ((struct Any){ &i, &j }) ;

  if (ap->type != &i || ap->addr != &j)
    return 1;
  return 0;
}

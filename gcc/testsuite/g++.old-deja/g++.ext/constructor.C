// Testcase for constructor expressions (GNU extension)
// Special g++ Options:

struct Any {
    int *type;
    int *addr;
};

int i, j;

main () {
  struct Any *ap = (struct Any *)
    __builtin_alloca (sizeof(struct Any));
  *ap = ((struct Any){ &i, &j }) ;

  if (ap->type != &i || ap->addr != &j)
    return 1;
  return 0;
}

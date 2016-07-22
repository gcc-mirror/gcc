
extern int __attribute__((weak)) decl;  /* { dg-error "weak declarations" } */
int __attribute__((weak)) defn;

int Foo ()
{
  return decl + defn;
}


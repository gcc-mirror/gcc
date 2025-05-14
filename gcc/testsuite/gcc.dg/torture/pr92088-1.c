/* { dg-do run } */

int __attribute__((noipa))
g (char *p)
{
  return p[9];
}
int main (int argc, char **argv)
{
  struct S {
    char toto[argc + 16];
  };
  int f (struct S arg) {
      __builtin_strcpy(arg.toto, "helloworld");
      return g (arg.toto);
  }
  struct S bob;
  __builtin_strcpy(bob.toto, "coucoucoucou");
  if (f(bob) != 'd' || __builtin_strcmp (bob.toto, "coucoucoucou"))
    __builtin_abort ();
  return 0;
}

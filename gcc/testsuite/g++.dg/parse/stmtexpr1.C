// { dg-do compile }
// { dg-options "" }

int
main (int argc, char **argv)
{
  int a = ({ 1 ? 0 : 1; });
  return ({ argc > 1 ? 1 : 0; });
}

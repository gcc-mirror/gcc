extern void main_test (void);
int inside_main;

int
main ()
{
  inside_main = 1;
  main_test ();
  inside_main = 0;
  return 0;
}

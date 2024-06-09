struct foo
{
  char a;
} foo[100];

int
main (void)
{
  foo[1].a = '1';
  foo[2].a = '2';
}

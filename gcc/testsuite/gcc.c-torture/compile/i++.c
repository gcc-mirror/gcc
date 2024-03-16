int main ()
{
  int i = 2;

  i = i++;
  __builtin_printf ("%d\n",i);
}

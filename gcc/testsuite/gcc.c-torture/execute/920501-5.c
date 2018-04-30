/* { dg-require-effective-target label_values } */

x (int i)
{
  void *j[] = {&&x, &&y, &&z};
  goto *j[i];
 x:return 2;
 y:return 3;
 z:return 5;

}
main ()
{
  if (x (0) != 2 || x (1) != 3 || x (2) != 5)
    abort();
  exit(0);
}

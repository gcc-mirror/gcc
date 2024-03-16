/* { dg-require-effective-target label_values } */

void abort (void);
void exit (int);

int
x (int i)
{
  void *j[] = {&&x, &&y, &&z};
  goto *j[i];
 x:return 2;
 y:return 3;
 z:return 5;

}
int
main (void)
{
  if (x (0) != 2 || x (1) != 3 || x (2) != 5)
    abort();
  exit(0);
}

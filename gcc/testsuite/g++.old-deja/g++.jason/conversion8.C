// { dg-do run  }
// PRMS id: 8279

int main ()
{
  char *const *p = 0;
  char **q = 0;

  (void)(p - q);
  (void)(q - p);
}

// PRMS id: 8279

main ()
{
  char *const *p = 0;
  char **q = 0;

  (void)(p - q);
  (void)(q - p);
}

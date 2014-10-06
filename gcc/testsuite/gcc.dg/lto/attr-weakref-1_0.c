/* { dg-lto-do run } */
int first = 0;
void abort (void);
void c (void);
void b (void);
int second = 0;
void callmealias (void)
{
  if (!first || !second)
   abort ();
}
void callmefirst (void)
{
  if (first)
    abort();
  first = 1;
}
void callmesecond (void)
{
  if (!first)
    abort();
  if (second)
    abort();
  second = 1;
}
int
main()
{
  c();
  b();
  return 0;
}

// { dg-lto-do run }

static const char *fname;
struct S
{
  S () { fname = __func__; }
};
extern "C" void abort (void);
int
main ()
{
  S tmp;
  if (fname[0] != 'S')
    abort ();
  return 0;
}

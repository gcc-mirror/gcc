void abort (void);
void exit (int);

int b = 0;

void func (void) { }

void
testit(int x)
{
  if (x != 20)
    abort ();
}

int
main()

{
  int a = 0;

  if (b)
    func();

  /* simplify_and_const_int would incorrectly omit the mask in
     the line below.  */
  testit ((a + 23) & 0xfffffffc);
  exit (0);
}

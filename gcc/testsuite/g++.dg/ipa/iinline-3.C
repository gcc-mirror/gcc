/* Verify that we do not indirect-inline using member pointer
   parameters which have been modified.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

extern "C" void abort (void);

class String
{
private:
  const char *data;

public:
  String (const char *d) : data(d)
  {}

  int funcOne (int stuff) const;
  int funcTwo (int stuff) const;
};


int String::funcOne (int stuff) const
{
  return stuff + 1;
}

int String::funcTwo (int stuff) const
{
  return stuff + 100;
}

int (String::* gmp)(int stuff) const = &String::funcTwo;

int docalling_1 (int (String::* f)(int stuff) const)
{
  String S ("muhehehe");

  return (S.*f)(4);
}

int docalling (int a, int (String::* f)(int stuff) const)
{
  if (a < 200)
    f = gmp;

  return docalling_1 (f);
}

int __attribute__ ((noinline,noclone)) get_input (void)
{
  return 1;
}

int main (int argc, char *argv[])
{
  int i = 0;
  while (i < 10)
    i += docalling (get_input (), &String::funcOne);

  if (i != 104)
    abort();
  return 0;
}

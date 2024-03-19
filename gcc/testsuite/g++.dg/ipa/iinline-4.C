/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

extern void non_existent (const char *, int);

class String
{
private:
  const char *data;

public:
  String (const char *d) : data(d)
  {}

  int funcOne (int delim) const;
  int printStuffTwice (int delim) const;
  virtual int whee (int delim) const;
};


int String::funcOne (int delim) const
{
  int i;
  for (i = 0; i < delim; i++)
    non_existent(data, i);

  return 1;
}

extern int global;

int docalling (int c, int (String::* f)(int delim) const)
{
  String S ("muhehehe");

  if (c > 2)
    global = 3;
  else
    global = 5;

  return (S.*f)(4);
}

int __attribute__ ((noinline,noclone)) get_input (void)
{
  return 1;
}

int main (int argc, char *argv[])
{
  int i = 0;
  while (i < 1000)
    i += docalling (get_input (), &String::funcOne);
  non_existent ("done", i);
  return 0;
}

/* { dg-final { scan-ipa-dump "String::funcOne\[^\\n\]*inline copy in int main"  "inline"  } } */

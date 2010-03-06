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
};


int String::funcOne (int delim) const
{
  int i;
  for (i = 0; i < delim; i++)
    non_existent(data, i);

  return 1;
}

int docalling (int (String::* f)(int delim) const)
{
  String S ("muhehehe");

  return (S.*f)(4);
}

int main (int argc, char *argv[])
{
  int i;
  i = docalling (&String::funcOne);
  non_existent ("done", i);
  return 0;
}

/* { dg-final { scan-ipa-dump "String::funcOne\[^\\n\]*inline copy in int main"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */

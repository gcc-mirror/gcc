// { dg-do run }
//

// Failed on powerpc64-linux for structure sizes > 64 and with size not a
// multiple of 8 after padding.
struct object
{
  int i1;
  char s1[60];
  int i2;
  char s2[64];
};

extern int subr (struct object obj);

int main ()
{
  struct object obj;

  obj.i1 = 1234;
  obj.i2 = 5678;
  return subr (obj);
}

int subr (struct object obj)
{
  return obj.i1 != 1234 || obj.i2 != 5678;
}

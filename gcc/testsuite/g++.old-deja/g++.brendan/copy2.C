// GROUPS passed copy-ctors
/*
The old g++ output is

Item()
Compound()
Pre foo
foo
~Compound()
~Item()
Post foo
~Compound()
~Item()

The output should be something like (produced from ATT 2.1)

Item()
Compound()
Pre foo
Item(const Item& i)    <------ missing above
foo
~Compound()
~Item()
Post foo
~Compound()
~Item()

*/

extern "C" void printf (char *, ...);
extern "C" void exit (int);

int count = 0;

void
die (int x)
{
  if (x != ++count)
    {
      printf ("FAIL\n");
      exit (1);
    }
}
  

class Item {
 public:
  Item() { die (1); }
  Item(const Item& i) { die (4); }
  ~Item() { count++; if (count != 7 && count != 10) die (-1); }
};


class Compound {
  Item i;
 public:
  Compound() { die (2); }
  ~Compound() { count++; if (count != 6 && count != 9) die (-1); }
};


void foo(Compound a)
{
  die (5);
}

int
main()
{
  Compound a;

  die (3);
  foo(a);

  die (8);

  printf ("PASS\n");
}


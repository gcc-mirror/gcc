// { dg-do run  }
// GROUPS passed nested-classes
#include <iostream>
#include <cstdio>
#include <cstring>

static char output[1024];

class BDDRetrace {
public:
    class Dump {
    public:
	virtual Dump& operator<<(char c) = 0;
	virtual Dump& operator<<(int i) = 0;
	virtual Dump& operator<<(double r) = 0;
    };

    class Dump1: public Dump {
    public:
	Dump& operator<<(char c);
	Dump& operator<<(int i);
	Dump& operator<<(double r);
    };
};

class Dump2: public BDDRetrace::Dump {
public:
    BDDRetrace::Dump& operator<<(char c);
    BDDRetrace::Dump& operator<<(int i);
    BDDRetrace::Dump& operator<<(double r);
};

BDDRetrace::Dump&
BDDRetrace::Dump1::operator<<(char c)
{ char tempout[1024];
  std::sprintf(tempout, "%s%s%c", output, "1-", c);
  std::strcpy(output, tempout);
  return *this;
}

BDDRetrace::Dump&
BDDRetrace::Dump1::operator<<(int i)
{ char tempout[1024];
  std::sprintf (tempout, "%s%s%d", output, "1-", i);
  std::strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
BDDRetrace::Dump1::operator<<(double r)
{ char tempout[1024];
  std::sprintf (tempout, "%s%s%1.0f", output, "1-", r);
  std::strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
Dump2::operator<<(char c)
{ char tempout[1024];
  std::sprintf (tempout, "%s%s%c", output, "2-", c);
  std::strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
Dump2::operator<<(int i)
{ char tempout[1024];
  std::sprintf (tempout, "%s%s%d", output, "2-", i);
  std::strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
Dump2::operator<<(double r)
{ char tempout[1024];
  std::sprintf (tempout, "%s%s%1.0f", output, "2-", r);
  std::strcpy (output, tempout);
  return *this; }

int main()
{
    BDDRetrace::Dump1 d1;
    Dump2 d2;

    std::sprintf (output, " ");

    d1 << 'a';
    d1 << 1;
    d1 << 1.0;

    d2 << 'a';
    d2 << 1;
    d2 << 1.0;

    if (std::strcmp (output, " 1-a1-11-12-a2-12-1") == 0)
      std::printf ("PASS\n");
    else
      { std::printf ("FAIL\n"); return 1; }

    return 0;
}

// GROUPS passed nested-classes
#include <iostream.h>
#include <stdio.h>

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
  sprintf (tempout, "%s%s%c", output, "1-", c);
  strcpy (output, tempout);
  return *this;
}

BDDRetrace::Dump&
BDDRetrace::Dump1::operator<<(int i)
{ char tempout[1024];
  sprintf (tempout, "%s%s%d", output, "1-", i);
  strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
BDDRetrace::Dump1::operator<<(double r)
{ char tempout[1024];
  sprintf (tempout, "%s%s%1.0f", output, "1-", r);
  strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
Dump2::operator<<(char c)
{ char tempout[1024];
  sprintf (tempout, "%s%s%c", output, "2-", c);
  strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
Dump2::operator<<(int i)
{ char tempout[1024];
  sprintf (tempout, "%s%s%d", output, "2-", i);
  strcpy (output, tempout);
  return *this; }

BDDRetrace::Dump&
Dump2::operator<<(double r)
{ char tempout[1024];
  sprintf (tempout, "%s%s%1.0f", output, "2-", r);
  strcpy (output, tempout);
  return *this; }

int main()
{
    BDDRetrace::Dump1 d1;
    Dump2 d2;

    sprintf (output, " ");

    d1 << 'a';
    d1 << 1;
    d1 << 1.0;

    d2 << 'a';
    d2 << 1;
    d2 << 1.0;

    if (strcmp (output, " 1-a1-11-12-a2-12-1") == 0)
      printf ("PASS\n");
    else
      printf ("FAIL\n");

    return 0;
}

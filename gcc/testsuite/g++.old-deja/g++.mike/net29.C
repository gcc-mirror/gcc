// { dg-do assemble  }
// This is a test case for the recent libg++ make check problem.

class SubString {
public:
  SubString();
  SubString(const SubString& x);
};

class String {
public:
  String();
  String(const SubString&  x);
};

int operator!=(const String& x, const SubString&  y);
int operator!=(const String& x, const String& y);

int operator!=(const SubString& x, const String& y);
int operator!=(const SubString& x, const SubString&  y);

void comparetest()
{  
  String x;
  SubString s;
  x != s;
}

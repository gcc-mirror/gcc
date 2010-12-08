// { dg-do assemble  }
// GROUPS passed enums
// enum file
// From: dougm@cs.rice.edu (Doug Moore)
// Date:     Thu, 18 Mar 93 00:14:57 CST
// Subject:  2.3.3:Inconsistent behavior for enum conversions
// Message-ID: <9303180614.AA12123@cs.rice.edu>

enum Enum {enumerator1, enumerator2};

struct Struct
{
  int i;
      int getI(Enum) {return i;} // { dg-message "Struct::getI|no known conversion" }
};

int funct (Enum)
{
  return 0;
}

int main()
{
  Enum e = enumerator1;
  Struct s;
  int x = funct(e+1);// { dg-error "invalid" }
  int y = s.getI(e+1);// { dg-error "match" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 27 }
  return x+y;
}

// { dg-do assemble  }
// { dg-options "-ffriend-injection -Wno-deprecated" }

class foo {
 public:
  friend int operator ^(const foo&, const foo&); // { dg-message "is visible" }
};

int main ()
{
   int (*funptr) (const foo &, const foo &)  = operator ^;
}

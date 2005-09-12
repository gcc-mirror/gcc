// { dg-do assemble  }
// { dg-options "-ffriend-injection" }

class foo {
 public:
   friend int operator ^(const foo&, const foo&);
};

int main ()
{
   int (*funptr) (const foo &, const foo &)  = operator ^;
}

// { dg-do assemble  }

class foo {
 public:
   friend int operator ^(const foo&, const foo&);
};

int main ()
{
   int (*funptr) (const foo &, const foo &)  = operator ^;
}

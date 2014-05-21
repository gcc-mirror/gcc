// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

class foo {
 public:
   class __iterator;		// { dg-message "declaration" }
   friend class __iterator;
   typedef __iterator const_iterator;
   virtual ~foo() { }
   __iterator begin();				// { dg-message "foo::begin|no known conversion for implicit" } 
};
static void iteratorTest(const foo &x)
{
   foo::const_iterator i = x.begin();		// { dg-error "incomplete type" "incomplete type" } 
   // { dg-error "const foo" "" { target *-*-* } 14 }
   for (; i; ++i)
      *i;
}

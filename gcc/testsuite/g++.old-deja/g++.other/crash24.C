// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

class foo {
 public:
   class __iterator;
   friend class __iterator;
   typedef __iterator const_iterator;
   virtual ~foo() { }
   __iterator begin();				// { dg-message "candidates" } 
};
static void iteratorTest(const foo &x)
{
   foo::const_iterator i = x.begin();		// { dg-error "incomplete type" "incomplete type" } 
   // { dg-error "no matching" "no matching" { target *-*-* } 14 }
   for (; i; ++i)
      *i;
}

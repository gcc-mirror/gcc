// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

class foo {
 public:
   class __iterator;
   friend class __iterator;
   typedef __iterator const_iterator;
   virtual ~foo() { }
   __iterator begin();				// ERROR - 
};
static void iteratorTest(const foo &x)
{
   foo::const_iterator i = x.begin();		// ERROR - 
   for (; i; ++i)
      *i;
}

//Build don't link:
//reported by Theodore Papadopoulo (Theodore.Papadopoulo@sophia.inria.fr)

namespace A {

   namespace B {

      template <class T1,class T2>
         struct B {
               static const unsigned count = 0;
               template <class ForwardIter>
               void SetError(ForwardIter it,const T1& p1,const T2& p2) const { }
         };

      template <>
         const unsigned B<int,int>::count = 2; // ERROR - duplicate init
   }
};

int
main()
{
   unsigned kk =  A::B<int,int>::count; // ERROR - not a template: syntax error
}

// Build don't link:
// Origin: Neil Booth, from PR #78

namespace MySpace
 {
   class Tag1 { };
   class Tag2 { };

   template<class Tag>
   class Object
   {
   public:

     friend void Forgotten(Object const & m) {}
   };

   typedef Object<Tag1> U1;
   typedef Object<Tag2> U2;

   void foo()
   {
     Forgotten(U1());
     Forgotten(U2());
   }

   void bar()
   {
     Forgotten(U1());
   }
 }

// PR c++/51188
// { dg-do compile }

#include <utility>
class XBase {
public:
   virtual ~XBase() = 0;
   enum ImpMode { Imp1, Imp2, Imp3 };
};
class X : public XBase {
   class XBlock {};
   using XBase::ImpMode;
   using XBase::Imp3;
   using XBase::Imp1;
   using XBase::Imp2;
   int _XBlocked;
   std::pair<int,int> getImp(void) const {
      return (std::make_pair(0, static_cast<int>(X::Imp1)));
   }
};

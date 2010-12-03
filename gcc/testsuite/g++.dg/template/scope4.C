// PR c++/46058

class StringLiterals {
public:
   static const char dec[];
};

template<class St, class Base, const char* name>
class NoValueCommand : public Base {
public:
};

template<class St, class Base>
class DecBasic : public NoValueCommand<St,Base,StringLiterals::dec> {
};

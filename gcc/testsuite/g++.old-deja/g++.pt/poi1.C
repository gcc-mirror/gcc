// { dg-do assemble  }
// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

template <class T>
class TLITERAL : public T 
    {
    int x;
    };

class GATOM;

typedef TLITERAL<GATOM> x;
extern TLITERAL<GATOM> y;



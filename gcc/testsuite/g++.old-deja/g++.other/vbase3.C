// { dg-do assemble  }
// Origin: scott snyder <snyder@fnal.gov>

class d0_Collection_Base {};

template <class T>
class d0_List
  : virtual public d0_Collection_Base
{
public:
  d0_List ();

  template <class Input_Iterator>
  d0_List (Input_Iterator first, Input_Iterator last)
    ;
};

void tlist ()
{
  const d0_List<int> l4 (1, 2);
}

// { dg-do assemble  }
// Origin: Alex Samuel <samuel@codesourcery.com>

namespace Outer 
{
template <class T> class Inner;
}

template <class T>
class Outer::Inner  
{
public:
  Inner ();
};

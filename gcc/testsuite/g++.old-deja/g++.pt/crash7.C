// { dg-do assemble  }

class foo 
{
};

template <class T : public foo> // { dg-error "" } base clause
struct bar
{
};

// Build don't link: 
// GROUPS passed templates
template <class T>
class Base
{
public:
    Base() { }
};

template <class memberType, class keyType>
class Middle : public Base<memberType>
{
public:
    Middle (keyType const & (*ko) (memberType const &))
    {
    }
};

// EXCHANGE the following lines for this code to work.
//template <class memberType, class keyType> class Middle;
template <class T, class keyType> class Middle;

struct Test : public Middle <int, int>
{
    Test();
};

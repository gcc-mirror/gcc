// Build don't link:

class foo 
{
};

template <class T : public foo> // ERROR - base clause
struct bar
{
};

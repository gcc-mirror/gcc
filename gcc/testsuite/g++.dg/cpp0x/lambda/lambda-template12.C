// PR c++/57568
// { dg-require-effective-target c++11 }

template < class T >
struct remove_reference
{ typedef int type; };
template < class T >
class X
{
    enum Q { };
    bool f ()
    {
        Q a;
        [&a]{
            typename remove_reference < decltype (a) >::type t;
        };
	return true;
    }
};
template class X< int >;

// { dg-options "" }
// { dg-do compile }
// C++/30221
// We would ICE while trying to reshape the pointer to
// member function element which is not needed.


class abstract {};
typedef void (abstract::*fptr1) (short & s ) const;
struct s {};
s array[] =
{
 (fptr1)0 
};// { dg-error "" }

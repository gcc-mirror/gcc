// Test use of `sizeof' as a template parameter.
// Origin: smacdonald@seimac.com

// { dg-do compile }

template <unsigned I> struct A { static char *value; };

template <typename SizeType>
struct B
{
char * f() const
{
return (A<sizeof(void *)>::value);
}
};

// Build don't link: 
// GROUPS passed error-reporting
// Special g++ Options: 
// Bug: # line directive gets ignored immediately after text.
template <class T> class A
{
public:
# 200 "lineno2.C"
};

main()
{
   undef1(); // ERROR - , LINE 204
}

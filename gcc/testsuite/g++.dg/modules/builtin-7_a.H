// { dg-additional-options -fmodule-header }
// { dg-module_cmi {} }

extern "C" 
{
extern double nan(const char *);
inline long double nanl(const char * __x) { return nan(__x); }
}

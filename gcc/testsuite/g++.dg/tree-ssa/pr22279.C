/* { dg-do compile } */
/* { dg-options "-O2" } */

struct string
{
  long long _M_p;
  long long i;
  string();
  int begin();
  int end();
  string(int, int);
};
struct symbol
{
  int type;
  string name;
  long long raw_name;
  long long demangled_name;
  long long version_name;
  int version_status;
  int status;
  void init();
};
void symbol::init() { name = string(); }
struct pair
{
  symbol first;
  symbol second;
  pair(const symbol& __a, const symbol& __b) : first(__a), second(__b) { }
};
struct vector
{
  void push_back(const pair& __x);
};
/* This ends up with two RHS deref copies, and we need to get the offsets right on them.  */
void f(vector incompatible)
{
  symbol base;
  incompatible.push_back(pair(base, base));
}




// Build don't link:
class ostream;
extern ostream cout;
namespace foo
{
  struct S
  {
    int i;
  };
  
  extern ostream &operator<<(ostream &, const S &);
}


void bar(foo::S s)
{
  cout << s ;
}

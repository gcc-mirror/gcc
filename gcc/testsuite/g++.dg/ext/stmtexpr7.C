// PR c++/24686
// { dg-options "" }

struct A
{
  ~A();
};
bool h(int, const A&);
int f();
int i;
void g()
{
  i && (A(), ({ static int l = f(); l; }));
}

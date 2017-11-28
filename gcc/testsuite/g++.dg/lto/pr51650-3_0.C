// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

struct T;
struct C 
{
    typedef ::T T;
    virtual void E();
    static T *m ()
      {
	static T *d;
	return d;
      }
};
int
fn ()
{
  C::m ();
  return 0;
}
int main() { return 0; }

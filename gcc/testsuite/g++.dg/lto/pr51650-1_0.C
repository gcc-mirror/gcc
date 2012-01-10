// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

struct T;
struct C 
{
    typedef ::T T;
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
}
int main() {}

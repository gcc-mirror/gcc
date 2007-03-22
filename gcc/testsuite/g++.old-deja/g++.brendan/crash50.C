// { dg-do assemble  }
// GROUPS passed old-abort
class B
        {
public:
        int i;
        };
int operator & (const B &s) { return ( s.i  );}





class C
        {
public:
        C &operator = (const C &x)
                {
                return *this;
                }
        };

C &(C::*DD)(const C &x) = &C::operator=;

int main()
{
        &DD;

}

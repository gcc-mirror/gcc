// The default assignment operator for B uses array assignment, so we can't
// just disallow it...

struct A { A& operator=(const A&); };
struct B { A f[20]; };

int a1[20], a2[20];
B b1, b2;

void
test ()
{
  b1 = b2;	      /* OK */
  a1 = a2;	      /* ERROR - array assignment */
}

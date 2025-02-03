// PR c++/117516
// { dg-do compile }

struct S10 { char a; };
#define A(m, n) struct S##m { struct S##n a, b; };
#define B(m) A(m##1, m##0) A(m##2, m##1) A(m##3, m##2) A(m##4, m##3) \
	     A(m##5, m##4) A(m##6, m##5) A(m##7, m##6) A(m##8, m##7)
B(1)
#define S20 S18
B(2)
#define S30 S28
B(3)
#define S40 S38
A(41, 40) A(42, 41) A(43, 42)

int
main ()
{
  struct S43 s;
  __builtin_memset (&s, 0, sizeof (s));
}

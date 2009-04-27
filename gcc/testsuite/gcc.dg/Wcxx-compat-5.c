/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
enum E1 { A, B, C };
enum E2 { D, E, F };

int f1() { return A; }

struct s { enum E1 e1 : 3; enum E2 e2 : 4; };

enum E1
f2 (int i, struct s sv, struct s *pv)
{
  int a;
  enum E1 e1 = B;
  enum E2 e2 = E;
  switch (i)
    {
    case 0:
      return A;
    case 1:
      return D;			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    case 2:
      return 0;			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    case 3:
      return (enum E1) 1;
    case 4:
      return (enum E2) 2;	/* { dg-warning "invalid in C\[+\]\[+\]" } */
    case 5:
      return e1;
    case 6:
      return e2;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
    case 7:
      return pv->e1;
    case 8:
      return sv.e1;
    case 9:
      return pv->e2;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
    case 10:
      return sv.e2;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
    case 11:
      return 1, A;
    default:
      return C;
    }
}

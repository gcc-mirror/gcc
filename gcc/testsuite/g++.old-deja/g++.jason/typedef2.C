// { dg-do run  }
// PRMS Id: 5367
// Bug: the nested name of C::func gets hosed.

struct C {
  typedef int func(int *, int *);
};

int
main()
{
   C::func *handler;
}

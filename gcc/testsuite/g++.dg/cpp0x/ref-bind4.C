// PR c++/91889 - follow-up fix for DR 2352.
// { dg-do compile { target c++11 } }

int i;

void f1 (int *);
void f1 (const int *const &);

void f2 (int *);
void f2 (const int *&);

void f3 (const int *);
void f3 (int *const &);

void f4 (int *&);
void f4 (int *const &);

void f5 (const int *&);
void f5 (int *const &);

void f6 (int *const &);
void f6 (const int *const &);

void f7 (int **const);
void f7 (const int *const *const &);

void f8 (const int *const *);
void f8 (const int *const *const &);

void f9 (int *const *);
void f9 (const int *const *const &);

void
g (int *p, const int *pc, const int **q)
{
  f1 (p);
  f1 (pc);
  f2 (p);
  f2 (pc);
  f3 (p);
  f3 (pc);
  f4 (p);
  f5 (p);
  f5 (pc);
  f6 (p);
  f6 (pc);
  f7 (q);
  /* [over.ics.rank]

   --S1 and S2 differ only in their qualification conversion and  yield
     similar  types  T1 and T2 (_conv.qual_), respectively, and the cv-
     qualification signature of type T1 is a proper subset of  the  cv-
     qualification signature of type T2  */
  f8 (q);
  f9 (q);
}

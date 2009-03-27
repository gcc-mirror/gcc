// PR tree-optimization/39557
// invalid post-dom info leads to infinite loop
// { dg-do run }
// { dg-options "-Wall -fno-exceptions -O2 -fprofile-use -fno-rtti" }

struct C
{
 virtual const char *bar () const;
};

struct D
{
 D () : d1 (0) { }
 C *d2[4];
 int d1;
 inline const C & baz (int i) const { return *d2[i]; }
};

struct E
{
 unsigned char e1[2];
 D e2;
 bool foo () const { return (e1[1] & 1) != 0; }
 virtual const char *bar () const __attribute__ ((noinline));
};

const char *
C::bar () const
{
 return 0;
}

C c;

const char *
E::bar () const
{
 const char *e = __null;
 if (foo () && (e = c.C::bar ()))
   return e;
 for (int i = 0, n = e2.d1; i < n; i++)
   if ((e = e2.baz (i).C::bar ()))
     return e;
 return e;
}

int
main ()
{
 E e;
 e.bar ();
} // { dg-message  "note: file" "" }

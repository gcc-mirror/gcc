// PR tree-optimization/17724
// { dg-do compile }
// { dg-options "-O2" }

extern "C" char *strcpy (char* d, const char* s);

class A { public: A (); ~A (); };

inline char * B (char *s, const char *t)
{ return ::strcpy (s, t); }

class C { int D (void); int E; };

int C::D (void)
{
  A a;
  try
    {
      char z[22];
      if (this->E) B (z, "");
      return 0;
    }
  catch (int &f) { return -1; }
}

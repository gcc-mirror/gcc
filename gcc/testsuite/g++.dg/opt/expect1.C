// PR c++/13239
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);

struct Y {
  int i;
};

bool foo () { return true; }
Y bar () { Y y = {0}; return y; }

int main ()
{
  __builtin_expect (foo () && (bar ().i) == 0, 0) ? 0 : (abort (), 1);
}

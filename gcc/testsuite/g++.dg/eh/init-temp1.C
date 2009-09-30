// PR c++/15764
// { dg-do run }

extern "C" void abort (); 
 
int thrown; 

int as;
struct a {
  a () { ++as; }
  ~a () { --as; if (thrown++ == 0) throw 42; }
}; 
 
int f (a const&) { return 1; } 
int f (a const&, a const&) { return 1; } 

int bs;
int as_sav;
struct b { 
  b (...) { ++bs; }
  ~b ()   { --bs; as_sav = as; }
}; 

bool p;
void g()
{
  if (p) throw 42;
}

int main () { 
  thrown = 0;
  try {
    b tmp(f (a(), a()));

    g();
  }  
  catch (...) {}

  // We throw when the first a is destroyed, which should destroy b before
  // the other a.
  if (as_sav != 1)
    abort ();

  thrown = 0;
  try {
    b tmp(f (a()));

    g();
  }  
  catch (...) {} 
 
  if (bs != 0)
    abort (); 
} 

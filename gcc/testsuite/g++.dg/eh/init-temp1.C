// PR c++/15764

extern "C" void abort (); 
 
int counter = 0; 
int thrown; 
struct a { 
  ~a () { if (thrown++ == 0) throw 42; } 
}; 
 
int f (a const&) { return 1; } 
int f (a const&, a const&) { return 1; } 
 
struct b { 
  b (...) { ++counter; } 
  ~b ()   { --counter; } 
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

  thrown = 0;
  try {
    b tmp(f (a()));

    g();
  }  
  catch (...) {} 
 
  if (counter != 0) 
    abort (); 
} 

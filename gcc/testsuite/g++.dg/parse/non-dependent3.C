// PR c++/14586

enum E { e }; 
 
E & operator |= (E &f1, const E &f2); 
 
E operator | (const E &f1, const E &f2) { 
  E result = f1; 
  result |= f2; 
  return result; 
} 
 
template <typename> void foo () { 
  const E flags = e | e; 
} 
 
template void foo<double> ();

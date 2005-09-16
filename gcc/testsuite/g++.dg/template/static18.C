// PR c++/23914

template <class T> 
struct foo_template { 
  static const unsigned complexity = 0; 
}; 
 
template <int x> struct STATIC_ASSERTION {}; 
 
void gcc_402_problem_minimal() 
{ 
  sizeof(STATIC_ASSERTION< foo_template<int>::complexity >); 
} 

// C++/15119
//  This ICEd in substitute_placeholder_in_expr because it did not
//  implement the 4 element trees.
//  Orginal test by: <wanderer@rsu.ru>
//  Reduced by: <bangerth@dealii.org>
/* { dg-do compile } */

template<typename T> 
const T& xmin(const T& a, const T& b); 
 
void bar (char *, ...); 
 
char const* descs[4]; 
 
int main() {    
    int t = 1; 
    char buf[100]; 
    bar( buf, descs[ xmin(t-1,4) ], 0 );  
    return 0; 
} 

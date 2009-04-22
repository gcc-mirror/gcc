// PR 14875: When using 'or' keyword, the error message speaks of a '||' token
// { dg-do compile }
// { dg-options "" }
using namespace std; 
 
class Sample 
{ 
 
public: 
  Sample(); 
  void or(long Digital);  // { dg-error "before .or. token" }
}; 
 
Sample::Sample() 
{ 
} 
 
void Sample::or(long Digital) // { dg-error "before .or. token" }
{ 
}

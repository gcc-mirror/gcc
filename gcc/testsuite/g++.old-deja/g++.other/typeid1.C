// Build don't link:
// Origin: sk@gluit.de
// Special g++ Options: 

#include <typeinfo>

int main ()
{
  typeid(char*);
  
  int len = 1;
  char carr[len];
  typeid(typeof(carr)); // ERROR - type has variable size
  typeid(carr); // ERROR - type has variable size
}

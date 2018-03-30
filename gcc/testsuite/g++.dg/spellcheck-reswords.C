void pr81610 (void *p)
{  
  forget (p); // { dg-error "not declared" }
  // { dg-bogus "'for'" "" { target *-*-*} .-1 }
}

void pr80567 (void *p)
{
  memset (p, 0, 4); // { dg-error "not declared" }
  // { dg-bogus "'else'" "" { target *-*-*} .-1 }
  // { dg-message "'#include <cstring>'" "" { target *-*-*} .-2 }
}

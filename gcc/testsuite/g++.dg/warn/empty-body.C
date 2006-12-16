// PR c++/5520
// { dg-options "-O2 -Wempty-body" }

void breakme()
{
  if(1) ;   // { dg-warning "empty body" }
  if(1) {}  // { dg-bogus "empty body" }
  if(1) (void)0; // { dg-bogus "empty body" }
  if(1) {} else; // { dg-warning "empty body" }
  if(1) {} else (void)0; // // { dg-bogus "empty body" }
  if(1) ;  else (void)0; // // { dg-bogus "empty body" }
}

// PR C++/21614
// { dg-additional-sources "ptrmem6a.C" }
// { dg-do run }

extern struct Z *p; 
extern int (Z::*m) (); 
 
int main () { 
  if ((p->*m)() == 7)
    return 0;
  return 1;
}

// { dg-do assemble  }
// { dg-options "-w" }
// GROUPS passed niklas hiding ARM
struct stat {};
stat gstat;
int stat (struct stat*);
void f () { struct stat* ps; stat (ps); }

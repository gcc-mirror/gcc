// { dg-do assemble  }
// GROUPS passed old-abort
struct wait { int w_status; };
int wait();
extern "C" int wait(int*);



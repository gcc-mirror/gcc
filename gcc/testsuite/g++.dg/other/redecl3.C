// PR c++/22556

extern int foo[]; // OK
int foo[] = {1,2,3};
extern int foo[]; // OK

void bar(){
  extern int foo[]; // g++: ERROR -- SHOULD BE OK
}

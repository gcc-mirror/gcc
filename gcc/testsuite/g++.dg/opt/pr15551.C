// PR target/15551
// This used to crash on pentium4-pc-cygwin due to an alloca problem.
// Testcase submitted by Hans Horn to mingw bug tracker
//
// { dg-do run }
// { dg-options "-O3" }

#include <cstring>
#include <fstream>
using namespace std;

ostream* logfile;

int main () {

  logfile = new ofstream("bar", ios::out);

  char expList[20000];
  strcpy(expList, "foo");

  delete logfile;
  unlink ("bar");

  return 0;
}

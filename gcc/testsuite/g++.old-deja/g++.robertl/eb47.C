/*
 *  Test program to isolate internal compiler error.
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <iostream.h>
#include <fstream.h>
#include <ctype.h>

#include <vector.h>

class MESSAGE {
public:
  int MessNum;
  int Size;

  // constructors
  MESSAGE(int MN, int Sz);

  MESSAGE();

};

//  Make a message if message rule is triggered by event.
//  Returns either a MESSAGE * (if successful) or NULL (if not).
MESSAGE *MakMessage(int ev, int sz);

int main(int argc, char **argv) {
  vector<MESSAGE &> Messages;
  vector<MESSAGE &>::iterator itMess;

  int MN, SZ;

  MN=SZ=1;

  MESSAGE *Messg=MakMessage(MN,SZ);
  if (Messg) Messages.push_back(*Messg);
}



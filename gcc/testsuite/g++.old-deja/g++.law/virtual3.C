// { dg-do run  }
// GROUPS passed virtual-functions
// virtual file
// From: allan@ramjet.multinet.DE (Allan Brighton)
// Subject: pos. bug in gcc-2.5.2 on hp
// Date: 4 Nov 1993 22:57:36 -0500
// Message-ID: <9311041820.AA05942@ramjet.multinet.DE>

#include <iostream>
#include <sstream>

using namespace std;

class BugStream : public ostringstream {
public:
    BugStream() {}
    BugStream& eval();
};


static struct Eval_ { } eval;
BugStream& operator<<(ostream& os, Eval_);

BugStream& BugStream::eval()
{
   // make sure str is null terminated
   *this << ends;
   
   // eval the command and set the status
   const char* s = str().data();
   cerr << s << endl;
   
   // reset the stream for the next command    
   clear(ios::goodbit);
   //   rdbuf()->freeze(0);
   seekp(0);
   
   return *this;
}

BugStream& operator<<(ostream& os, Eval_)
{
    return ((BugStream&)os).eval();
}

int main() {
    BugStream bs;
    bs << "PASS" << eval;
}

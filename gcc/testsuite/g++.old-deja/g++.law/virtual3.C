// GROUPS passed virtual-functions
// virtual file
// From: allan@ramjet.multinet.DE (Allan Brighton)
// Subject: pos. bug in gcc-2.5.2 on hp
// Date: 4 Nov 1993 22:57:36 -0500
// Message-ID: <9311041820.AA05942@ramjet.multinet.DE>

#include <iostream.h>
#include <strstream.h>


class BugStream : public ostrstream {
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
   char* s = str();
   cerr << s << endl;
   
   // reset the stream for the next command    
   clear(0);
   rdbuf()->freeze(0);
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

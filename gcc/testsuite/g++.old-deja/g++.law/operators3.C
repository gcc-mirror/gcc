// { dg-do assemble  }
// GROUPS passed operators
// opr-pl file
// Message-Id: <9212010409.AA24967@zeus.research.otc.com.au>
// From: grahamd@zeus.research.otca.oz.au (Graham Dumpleton)
// Subject: Failure to use conversion operator.
// Date: Tue, 1 Dec 92 15:11:18 EST

class BUG1
{
  public:

    operator char*() const { return myData; }
    char* myData;
};

void bug1()
{
  BUG1 bug1;
  bug1.myData = "0123456789";
  char* s = bug1 + 1;
}

// { dg-do assemble  }
// GROUPS passed overloading
class CLogger
{
public:
      void operator() (int,const char *) {}; // { dg-error "" } candidates
      void operator() (int,const char *, ...) {}; // { dg-error "" } candidates
} Log;

class CGLogger : public CLogger
{
} GLog;

int main()
{
        Log(1,"Test");// { dg-error "" }  call of.*
        Log(1,"Test %d",3);
        GLog(1,"Test");// { dg-error "" }  call of.*
        GLog(1,"Test %d",3);
}

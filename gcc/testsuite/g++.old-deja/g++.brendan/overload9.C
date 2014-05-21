// { dg-do assemble  }
// GROUPS passed overloading
class CLogger
{
public:
      void operator() (int,const char *) {}; // { dg-message "note" }
      void operator() (int,const char *, ...) {}; // { dg-message "note" }
} Log;

class CGLogger : public CLogger
{
} GLog;

int main()
{
        Log(1,"Test");// { dg-error "ambiguous" }
        Log(1,"Test %d",3);
        GLog(1,"Test");// { dg-error "ambiguous" }
        GLog(1,"Test %d",3);
}

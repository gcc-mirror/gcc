// Build don't link: 
// GROUPS passed overloading
class CLogger
{
public:
      void operator() (int,const char *) {}; // ERROR - candidates
      void operator() (int,const char *, ...) {}; // ERROR - candidates
} Log;

class CGLogger : public CLogger
{
} GLog;

int main()
{
        Log(1,"Test");// ERROR -  call of.*
        Log(1,"Test %d",3);
        GLog(1,"Test");// ERROR -  call of.*
        GLog(1,"Test %d",3);
}

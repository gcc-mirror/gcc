// This tests two things:
// 1. there is an annoying warning.
// singleton.C:26: warning: `class singleton' only defines private constructors and has no friends
// egcs fails to see that there is a public static accessor function.
// 2. the program crashes, because apparently the static variable s in
// singleton::instance() is considered constructed although the ctor
// exited via an exception. (crash changed to non-zero return here)

class singleton {
public:
       static singleton& instance() {
               static singleton s;
               return s;
       }
       int check() {return initialized;}

private:
       singleton() : initialized(1) {
               if ( counter++ == 0 ) throw "just for the heck of it";
               initialized = 2;
       }
       singleton( const singleton& rhs );
       void operator=( const singleton& rhs );
       int initialized;
       static int counter;
};  

int singleton::counter;

int main()
{
       while (1) {
               try {
                       return singleton::instance().check()-2;
               } catch (...) { }
       }
}


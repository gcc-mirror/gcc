// This tests two things:
// 1. there is an annoying warning. singleton.C:27: warning: `class
// singleton' only defines private constructors and has no friends egcs
// fails to see that there is a public static accessor function.
// 2. the program crashes, because apparently the static variable s in
// singleton::instance() is considered constructed although the ctor
// exited via an exception.

class singleton {
public:
       static singleton& instance() {
               static singleton s;
               return s;
       }
       ~singleton() { delete sigsegv; }
       int crash() { return *sigsegv; }

private:
       singleton() : sigsegv(0) {
               if ( counter++ == 0 ) throw "just for the heck of it";
               sigsegv = new int(0);
       }
       singleton( const singleton& rhs );
       void operator=( const singleton& rhs );
       int* sigsegv;
       static int counter;
};

int singleton::counter;

int main()
{
       while (1) {
               try {
                       return singleton::instance().crash();
               } catch (...) { }
       }
}

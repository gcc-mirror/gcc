// PRMS id: 6863

extern "C" int printf(const char *, ...);
extern "C" void abort();

enum ENUM {E1=0, E2 };
int d;

class AAA{
public:
 AAA() {a = new char[10];printf("constructor AAA() called\n");}
 AAA(int) {printf("constructor AAA(int) called\n");}
 ~AAA(){ printf("destructor ~AAA() called\n"); d = 1; }
 operator int () { return 1;}
 char *a;
 int i;
};

struct sentinel {
  ~sentinel () { if (d == 0) abort (); }
} s;

/* forward decl here causes gcc not to execute ct and dt for arr1 */
extern AAA arr1[];

AAA arr1[] = {(int)E1 };

int main()
{
  return 0;
}

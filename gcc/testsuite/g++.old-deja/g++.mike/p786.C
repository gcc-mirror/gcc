// prms-id: 786

extern "C" int printf (const char *, ...);
extern "C" void exit(int);
class C
   {
   int a;
public:
   C() {a = 1;}
   };

void func(const C& a, C& b)
{
   printf ("in const func\n");
   exit(1);
}

void func(C& a, C& b)
{
   printf ("in non-const func\n");
}

void testit(const C& a, C& b)
{
   func(a,b);
}

int main()
{
   C a;
   C b;

   func(a,b);
   return 0;
}

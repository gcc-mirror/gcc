// PRMS Id: 4143
// Bug: Pointer is silently dereferenced in method call.
// Build don't link:

extern "C" int printf (const char *, ...);

class Test
{
    char ch;
  public:
    Test(char c) : ch(c) {}
    void Print() { printf("%c", ch); }
};

int main()
{
    Test *p = new Test('x');

    p.Print();	// ERROR - 
}

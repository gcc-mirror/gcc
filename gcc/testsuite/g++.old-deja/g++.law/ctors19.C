// Build don't link: 
// GROUPS passed constructors
// ctor file
// From: mkohtala@vinkku.hut.fi
// Date: Tue, 5 Oct 1993 19:31:16 +0200
// Message-Id: <199310051731.AA12260@lk-hp-11.hut.fi>
// Subject: Nested class constructor calling bug

class X
{
  public:
    class Y
    {
      public:
        Y(int i) : a(i) {}
        int a;
    };
    static void f(Y y);
};

void X::f(X::Y y)
{
}

int
main()
{
    X::Y y = X::Y(1);   // Tries to call ctor Y instead of X::Y
    X::f(X::Y(2));      // Tries to call Y instead of X::Y

    return 0;
}


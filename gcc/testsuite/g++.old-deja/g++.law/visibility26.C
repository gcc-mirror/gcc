// Build don't link: 
// GROUPS passed visibility
// visibility file
// From: jamshid@ses.com (Jamshid Afshar)
// Date:     Sun, 12 Dec 93 03:09:15 CST
// Subject:  Missed access declaration error
// Message-ID: <9312120909.AA22135@ses.com>

class X {
  public:
    void f();
};

class Y : private X {
  public:
    void f(int);
    X::f;  // used to be an error; now equivalent to 'using X::f'
};

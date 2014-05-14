// { dg-do assemble  }
// GROUPS passed constructors
// ctor file
// Message-Id: <9302081631.AA14744@tera.com>
// From: rrh@tera.com (Robert R. Henry)
// Date: Mon, 8 Feb 93 08:31:39 PST
extern "C" int printf (const char *, ...);
class A{
public:
  inline A(int x){printf("constructing A with %d\n", x);}
};

class B:public A{ // { dg-message "note" } non-default constructor
private:
public:
};

int main()
{
  B(10);// { dg-error "match" } B doesn't have a constructor taking int
}

// PR c++/50618
// { dg-options "-fdump-rtl-expand" }
// { dg-do run }

struct Base
{
    const int text;
    Base():text(1) {}
    Base(int aText)
    : text(aText) {}
};
struct SubA : public virtual Base
{
protected:
  int x;
public:
  SubA(int aX)
  : x(aX) {}
};
class SubB : public virtual Base
{};
struct Diamond : public SubA, public SubB
{
    Diamond(int text)
    : Base(text), SubA(5), SubB() {}

    void printText()
    {
        if(text != 2)
          __builtin_abort();
        if(x!=5)
          __builtin_abort();
    }
};

int main(int, char**)
{
    Diamond x(2);
    x.printText();
}

// Verify that the SubB() mem-initializer is storing 0 directly into
// this->D.whatever rather than into a stack temp that is then copied into the
// base field.
// { dg-final { scan-rtl-dump "set \[^\n\]*\n\[^\n\]*this\[^\n\]*\n\[^\n\]*const_int 0" "expand" { target { i?86-*-* x86_64-*-* } } } }

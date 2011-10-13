// PR c++/50618
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

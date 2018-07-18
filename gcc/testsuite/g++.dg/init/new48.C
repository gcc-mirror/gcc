// PR c++/78139

struct A
{
  A(int);
private:
  ~A();
};

struct B
{
  B(void*);
};

int main()
{
  B(new A(42));
}

// PR c++/48489

struct Base{ };

struct Concrete : Base 
{
  void setValue();
};

int main()
{
  Concrete d;
  d.Base::setValue(); // { dg-error "struct Base" }
}

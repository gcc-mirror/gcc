class Foo {
};

class Bar : virtual Foo {
public:
  int b;
} x;

int main()
{
  // printf("Foo offset %d\n", (int)(Foo*)&x - (int)&x);
  // printf("b offset %d\n", (int)&x.b - (int)&x);
  // printf("sizeof is %d\n", sizeof(Bar));
  // This core dumps on a SPARC is alignment is wrong.
  Bar blist[10];
}

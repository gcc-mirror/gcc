/* { dg-do compile } */

class mx {
public:
    mx();
};

int main()
{
  while (true) {
      mx *bar = new mx;
      mx *baz = new mx;
      continue;
  }
  return 0;
}

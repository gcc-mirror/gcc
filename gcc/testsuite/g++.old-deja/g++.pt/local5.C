// { dg-do run  }
template <class INT>
class b
{
private:
  char a(int x)
  {
    union {
      int i;
      char c;
    } val;
    val.i = x;
    return val.c;
  }

public:
  b()  {
  }
};

int main() {
  b<int> n;
  return 0;
}
   

// Test matching of partial specializations.

template <int* x, int* y>
class EQUAL {
public:
	enum { value = 0 };
};
template <int* x>
class EQUAL<x,x> {
public:
	enum { value = 1 };
};

int x;
int y;

int equals_x_x = EQUAL<&x,&x>::value; // expected value: 1
int equals_x_y = EQUAL<&x,&y>::value; // expected value: 0
int equals_y_x = EQUAL<&y,&x>::value; // expected value: 0
int equals_y_y = EQUAL<&y,&y>::value; // expected value: 1

int main ()
{
  if (equals_x_x == 1
      && equals_x_y == 0
      && equals_y_x == 0
      && equals_y_y == 1)
    return 0;
  return 1;
}

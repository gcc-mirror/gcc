// { dg-do run  }
// prms-id: 6611

class COMPLEX {
public:
  COMPLEX(double a, double b=0) { re = a; im = b; }
  void print() const { }
private:
  double re;
  double im;
};

int main(void)
{
  COMPLEX a[3][3] = {
    { 1, COMPLEX(2,3), COMPLEX(3,4), },
    { 1, COMPLEX(2,3), COMPLEX(3,4), },
    { 1, COMPLEX(2,3), COMPLEX(3,4), },
  };
  int i,j;

  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      a[i][j].print();
    }
  }
}

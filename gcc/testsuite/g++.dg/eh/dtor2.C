// PR c++/12751
// tree-ssa eh lowering bug ran a destructor twice for one object
// { dg-do run }

static int check;

struct Y {
  Y();
  ~Y();
};

void foo() {
  Y y1;
  Y y2;
  switch(0) {
    case 1: {
        Y y3;
        return;
      }
  }
}

Y::Y() { ++check; }
Y::~Y() { --check; }

int main()
{
  foo ();
  return check;
}

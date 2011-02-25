// { dg-options -std=c++98 }

void f () {
  switch (0) {
  case (3, 0): // { dg-error "" }
    break;
  }
}

int g ();

struct S {
  int i : (false ? g () : 1); // { dg-error "" }
};


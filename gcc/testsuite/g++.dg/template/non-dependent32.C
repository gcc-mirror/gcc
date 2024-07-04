// PR c++/114994
// { dg-do compile { target c++11 } }

struct udl_arg {
  udl_arg operator=(int);
};

void f(udl_arg&&);

template<class>
void g() {
  udl_arg x;
  f(x=42); // { dg-bogus "cannot bind" }
}

int main() {
  g<int>();
}

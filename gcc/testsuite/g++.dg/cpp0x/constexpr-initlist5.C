// PR c++/50024
// { dg-do compile { target c++11 } }

template< class T >
struct Container
{
  Container(){
    int* ptr = new int{};
  }
};

int main() {
    Container< int > c;
}


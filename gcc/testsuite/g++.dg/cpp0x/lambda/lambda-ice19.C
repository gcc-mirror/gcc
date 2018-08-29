// PR c++/61362
// { dg-do compile { target c++11 } }

struct function {
  template<class F>
  function(F){}

  void operator()(...) {}
};

struct Node {
  unsigned length;
};

template<typename N>
class C {
public:
  unsigned longest = 0;
  function f = [this](N node) {
    if(node->length > this->longest) this->longest = node->length;
  };
};

int main() {
  Node n;
  n.length = 5;
  C<Node*> c;
  c.f(&n);
}

// PR c++/25836

template <class T>
class Iter {};

template <class T>
class SubIter : public Iter<T> {
  void insert(T);
};

class GraphBase {
public:
  class Node;
};

template<class T>
class Graph : public GraphBase {
  class Inner {
    Iter<typename Graph<T>::Node*> *get();
  };
};

template<class T>
Iter<typename Graph<T>::Node*> *Graph<T>::Inner::get() {
  SubIter<typename Graph<T>::Node*> *iter;
  iter->insert(0);
}

int main() {
  Iter<Graph<int>::Node*> *n2_iter = new SubIter<Graph<int>::Node*>();
}

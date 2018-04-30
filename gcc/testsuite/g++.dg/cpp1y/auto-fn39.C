// PR c++/81045
// { dg-do compile { target c++14 } }

template<typename T> class vectorIterator;

template<typename T> class vector {
  public:
  auto get(unsigned int i) { return data[i]; }

  auto begin() { return vectorIterator<T>{*this, 0}; }
  auto end() { return vectorIterator<T>{*this, 10}; }

  private:
  T data[10] = {};
};

template<typename T> class vectorIterator {
  public:
  vectorIterator(vector<T>& self, unsigned int offset) : self(self), offset(offset) {}

  auto operator*() -> T& { return self.get(offset); }
  auto operator!=(const vectorIterator& source) -> bool { return offset != source.offset; }
  auto operator++() -> vectorIterator& { ++offset; return *this; }

  private:
  vector<T>& self;
  unsigned int offset;
};

class Object {
  public:
  template<typename T> auto cast() -> T {
    return T();
  }
};

class Group : public Object {
  public:
  template<typename T = Object> auto objects() const -> void {
    vector<Object> easyObjects;
    for(auto obj : easyObjects) {
      auto casted = obj.cast<T>();
    }
  }
};

int main() { return 0; }


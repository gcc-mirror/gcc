// { dg-do assemble  }

class K {
public:
  friend class C;

private:
  static K qwe;
  K();
  ~K();
}; 

K K::qwe;

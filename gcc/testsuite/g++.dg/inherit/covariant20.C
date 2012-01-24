// PR c++/51812
// { dg-do link }

class Object {
  virtual Object* clone() const;
};
class DNA: virtual public Object {
  virtual DNA* clone() const {return new DNA(*this);}
};
int main() { }

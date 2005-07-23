// { dg-do run  }
// This is a test case to make sure the explicit cast on a pointer to
// a member function works ok.
// prms-id: 3060

extern "C" int printf(const char *, ...);

class Object;

typedef void (Object::*VoidObjMemberFunc)(Object *tracker, void *ap);

class Object {
public:
  int         foo;
};

class Clipper: public Object {
public:
  int         bar;
  void        Feedback(Object*, void*);
};
void Clipper::Feedback(Object *tracker, void *ap) {
  printf("Doing feedback\n");
}

void vfunc(VoidObjMemberFunc of, Object *op, void *v1) {
  (op->*of)(op, v1);
}

int main() {
  Object      o;

  vfunc((VoidObjMemberFunc)&Clipper::Feedback, &o, 0);
  return 0;
}

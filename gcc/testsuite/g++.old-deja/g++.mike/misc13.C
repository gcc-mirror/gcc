// { dg-do run  }
// GROUPS passed vtable
extern "C" int printf (const char *, ...);
enum E { vf_request, vf_event } want;

int errs = 0;

class ivResource {
public:
  virtual ~ivResource () { }
};

class ivHandler   : public ivResource   {
public:
  virtual void event() { }
};

class ivGlyph   : public ivResource   {
public:
  virtual ~ivGlyph  () { }
  virtual void request () {
    if (want!=vf_request)
      ++errs;
  }
};

class ItemView : public ivGlyph, public ivHandler {
public:
  virtual void event () {
    if (want!=vf_event)
      ++errs;
  }
} a;

ivGlyph *bar() {
  return &a;
}

ivHandler *bar2() {
  return &a;
}

int main() {
  want=vf_request;
  bar()->request();
  want=vf_event;
  bar2()->event();
  if (errs) {
    printf("FAIL\n");
    return 1;
  }
  printf("PASS\n");
  return 0;
}

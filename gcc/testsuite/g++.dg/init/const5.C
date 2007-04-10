// PR c++/31449

class Foo {};
class Bar : public Foo {};
static const Foo *foo = 0;

static Bar *bar = static_cast<const Bar*>(foo); // { dg-error "conversion" }

void func(const Foo *foo) {
  Bar *bar = static_cast<const Bar*>(foo);  // { dg-error "conversion" }
}

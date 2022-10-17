// REQUIRED_ARGS: -Icompilable/imports
// EXTRA_FILES: imports/test19750a.d imports/test19750b.d imports/test19750c.d imports/test19750d.d
import test19750b;
class Foo {
  import test19750a;
  void func (Bar ) {}
}

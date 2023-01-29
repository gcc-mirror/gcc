module imports.test23490zoo;
import imports.test23490pop;
import imports.test23490frop;
class Foo23490() : Pop23490 {
  override void frop(Frop23490 f) {
    f.frolick;
  }
}
class Baz23490 {
  Foo23490!() foo;
  Frop23490 frop;
}
class Bar23490 {
  static instance() { return new Baz23490; }
  auto ss = __traits(getAttributes, instance.frop);
}
class Zoo23490 { }

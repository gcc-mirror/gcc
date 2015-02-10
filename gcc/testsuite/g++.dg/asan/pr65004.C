// PR sanitizer/65004
// { dg-do compile }
// { dg-options "-fcompare-debug -fsanitize=address -fsanitize=undefined -fno-sanitize-recover=all" }

namespace N {
  template <typename Signature> struct function;
  namespace detail {
    namespace function {
      struct vtable_base { };
    }
  }
  struct function_base {
    detail::function::vtable_base * vtable;
  };
  template <typename R, typename T0> struct function1 : public function_base { };
  template <typename R, typename T0> struct function <R (T0)> : public function1 <R, T0> { };
}
namespace Bar {
  typedef N::function <void (const char *)> WarningHandler;
}
namespace Foo {
  struct FooRecord {
    virtual ~FooRecord ();
  };
  struct TestRecord : public FooRecord {
    long x;
  };
}
namespace Foo {
  using Bar::WarningHandler;
  struct FooScanner {
    WarningHandler warnHandler;
    int readByte ();
    long readSignedInteger ();
  };
  struct FooRecordReader {
    FooScanner & scanner;
    long readSInt ();
    void readTestRecord (TestRecord * recp);
  };
  inline long FooRecordReader::readSInt () {
    return scanner.readSignedInteger ();
  }
  void FooRecordReader::readTestRecord (TestRecord * recp) {
    int infoByte = scanner.readByte ();
    recp->x = readSInt ();
  }
}

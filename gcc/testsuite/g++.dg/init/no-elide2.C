// PR c++/100838
// { dg-do run }
// { dg-additional-options -fno-elide-constructors }

extern "C" int puts (const char *);

int c,d;
class MyString {
public:
  MyString(const char* s = "") {
    puts ("ctor");
    ++c;
  }
  ~MyString() {
    puts ("dtor");
    ++d;
  }
  MyString(const MyString& s) {
    puts ("copy ctor");
    ++c;
  }
  MyString& operator=(const MyString& s);
};

int main() {
  {
    MyString s1 = "Hello";
    puts ("main");
  }
  if (c != d)
    __builtin_abort();
}

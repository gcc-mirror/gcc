// PR c++/118147
// { dg-do compile { target c++11 } }

struct F {
  int i = []{
    #pragma message "test"  // { dg-message "test" }
    return 1;
  }();
};

struct G {
  int i =
    #pragma GCC diagnostic push  // { dg-error "file ends in default argument|expected" }

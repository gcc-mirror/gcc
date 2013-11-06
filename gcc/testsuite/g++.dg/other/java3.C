// PR c++/11006

typedef int* jclass;

void foo () {
  new __java_boolean;  // { dg-error "valid" }
}

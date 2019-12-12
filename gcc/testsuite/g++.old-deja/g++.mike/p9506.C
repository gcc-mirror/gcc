// { dg-do assemble  }
// prms-id: 9506

char * volatile p;
void foo() {
  --p = 0; // { dg-warning "deprecated" "" { target c++2a } }
}

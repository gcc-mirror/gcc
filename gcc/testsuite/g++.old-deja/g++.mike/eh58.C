// { dg-do assemble  }
// { dg-options "-fexceptions" }

struct C {
    bool mem;
    ~C();
};

C genTemp();
  
int foo_notok(int arg) {
    switch (arg) {
    case 0:
        return genTemp().mem;
    default:
        return 3;
    }
}

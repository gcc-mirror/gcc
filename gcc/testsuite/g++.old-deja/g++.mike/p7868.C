// { dg-do assemble  }
// prms-id: 7868

struct DIAGTYP {
};
struct DIAGTYP1 {
  struct DIAGTYP;       // { dg-error "" } forward declaration
  void bar() { new struct DIAGTYP; }	// { dg-error "" } undefined
  void foo() { new struct DIAGTYP1; }
};

int main () {
  struct DIAGTYP;               // { dg-error "" } forward declaration
  struct DIAGTYP  *lerror_desc;
  lerror_desc= new struct DIAGTYP;	// { dg-error "" } undefined
}

void foo () {
  struct DIAGTYP  *lerror_desc;
  lerror_desc= new struct DIAGTYP;
}

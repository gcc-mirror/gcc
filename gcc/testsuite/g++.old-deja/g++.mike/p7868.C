// Build don't link:
// prms-id: 7868

struct DIAGTYP {
};
struct DIAGTYP1 {
  struct DIAGTYP;       // ERROR - forward declaration
  void bar() { new struct DIAGTYP; }	// ERROR - undefined
  void foo() { new struct DIAGTYP1; }
};

int main () {
  struct DIAGTYP;               // ERROR - forward declaration
  struct DIAGTYP  *lerror_desc;
  lerror_desc= new struct DIAGTYP;	// ERROR - undefined
}

void foo () {
  struct DIAGTYP  *lerror_desc;
  lerror_desc= new struct DIAGTYP;
}

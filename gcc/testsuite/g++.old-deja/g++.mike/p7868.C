// Build don't link:
// prms-id: 7868

struct DIAGTYP {
};
struct DIAGTYP1 {
  struct DIAGTYP;
  void bar() { new struct DIAGTYP; }	// ERROR - undefined
  void foo() { new struct DIAGTYP1; }
};

int main () {
  struct DIAGTYP;
  struct DIAGTYP  *lerror_desc;
  lerror_desc= new struct DIAGTYP;	// ERROR - undefined
}

void foo () {
  struct DIAGTYP  *lerror_desc;
  lerror_desc= new struct DIAGTYP;
}

// { dg-do run  }
// prms-id: 5840

class Signal {
public:
  int Name(void) { return 1; }
};

class Derived : public Signal {
public:
  int Name(void) { return 2; }
};

template <class Foo , int (Foo::*Id)(void)>
class Bar
{
public:
  int value (Foo* a) { return (a->*Id)(); }
};

/* The following line is illegal under the new rules for non-type
   template arguments in the standard, so it is commented out.  */
/* template class Bar <Derived, &Signal::Name>; */
template class Bar <Signal, &Signal::Name>;
template class Bar <Derived, &Derived::Name>;

Derived a;

/* Bar<Derived, &Signal::Name> dispatcher1; */
Bar<Derived, &Derived::Name> dispatcher2;

int main() {
  /* int i1 = dispatcher1.value(&a); */
  int i2 = dispatcher2.value(&a);
  return /* i1 != 1 || */ i2 != 2;
}

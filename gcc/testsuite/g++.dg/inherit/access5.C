struct S { ~S(); };
struct T : virtual private S {};
struct U : private T {};
U u;

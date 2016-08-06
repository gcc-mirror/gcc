struct A {
   A operator=(const A&);
};

A operator*(A, A);

A& operator+=(A& a, const A& b)
{
   return a = a * b;            // { dg-error "non-const lvalue reference" }
}

// PR c++/21352

struct coperator_stack
{
 template<class type>
 void push3()
 {
 }
};

struct helper {};

template<class F>
void bla(F f) // { dg-message "bla|no known conversion" }
{
}

template <typename ScannerT>
struct definition
{
 definition()
 {
   bla(coperator_stack::push3<helper>); // { dg-error "matching" }
   // { dg-message "candidate" "candidate note" { target *-*-* } 23 }
 }
};


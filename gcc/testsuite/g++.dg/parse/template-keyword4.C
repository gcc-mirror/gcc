// PR c++/104608
// { dg-do compile { target c++11 } }

class S;
using Parameter = S;
typedef S Parameter2;

template <typename R> class Function 
: public R  
{
    Function();
};
template <typename R>
Function<R>::Function() {
    this->template Parameter<R>();
    this->template Parameter2<R>();
}

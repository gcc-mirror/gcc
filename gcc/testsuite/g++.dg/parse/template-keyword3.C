// PR c++/104608

class Parameter;
template <typename R> class Function 
: public R  
{
    Function();
};
template <typename R>
Function<R>::Function() {
    this->template Parameter<R>();
}

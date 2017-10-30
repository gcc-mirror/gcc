// PR c++/82218
// { dg-do compile { target c++14 } }

template<typename _Tp>
struct identity
{
  typedef _Tp type;
};

template<typename _Tp>
inline _Tp&&
forward(typename identity<_Tp>::type&& __t)
{ return __t; }

template < typename T >
class delegate;

template < typename R, typename... Params >
class delegate< R(Params...) > final
{
private:
  using CallbackType = R (*)(void*, Params...);

  using FunctionPtr = R (*)(Params...);

  template < typename Object >
  using MethodPtr = R (Object::*)(Params...);

  template < typename Object >
  using ConstMethodPtr = R (Object::*)(Params...) const;

  void* obj_;
  CallbackType cb_;

  template < typename Object, MethodPtr< Object > Mptr >
  constexpr static R invoke_method(void* obj, Params... params) noexcept(
      noexcept((static_cast< Object* >(obj)->*Mptr)(params...)))
  {
    return (static_cast< Object* >(obj)->*Mptr)(params...);
  }

  template < typename Object, ConstMethodPtr< Object > Mptr >
  constexpr static R invoke_method(void* obj, Params... params) noexcept(
      noexcept((static_cast< Object* >(obj)->*Mptr)(params...)))
  {
    return (static_cast< Object* >(obj)->*Mptr)(params...);
  }

  template < FunctionPtr Fptr >
  constexpr static R invoke_function(void*, Params... params) noexcept(
      noexcept((*Fptr)(params...)))
  {
    return (*Fptr)(params...);
  }

  constexpr delegate(void* obj, CallbackType callback) noexcept : obj_(obj),
                                                                  cb_(callback)
  {
  }

  constexpr static R error_function(Params...)
  {
    while(1);
  }

public:
  using base_type = delegate< R(Params...) >;

  delegate()
  {
    *this = from< error_function >();
  }

  delegate(const base_type&) = default;
  delegate(base_type&&)      = default;

  base_type& operator=(const base_type&)  = default;
  base_type& operator=(base_type&&)       = default;

  template < typename Object, MethodPtr< Object > Mptr >
  constexpr static auto from(Object& obj) noexcept
  {
    return delegate(&obj, &invoke_method< Object, Mptr >);
  }

  template < typename Object, ConstMethodPtr< Object > Mptr >
  constexpr static auto from(Object& obj) noexcept
  {
    return delegate(&obj, &invoke_method< Object, Mptr >);
  }

  template < FunctionPtr Fptr >
  constexpr static auto from() noexcept
  {
    static_assert(Fptr != nullptr, "Function pointer must not be null");

    return delegate(nullptr, &invoke_function< Fptr >);
  }

  template < typename... Args >
  constexpr auto operator()(Args&&... params) const
      noexcept(noexcept((*cb_)(obj_, forward< Args >(params)...)))
  {
    return (*cb_)(obj_, forward< Args >(params)...);
  }

  constexpr bool valid() const noexcept
  {
    return (cb_ != &invoke_function< error_function >);
  }

  constexpr bool operator==(const delegate& other) const noexcept
  {
    return (obj_ == other.obj_) && (cb_ == other.cb_);
  }

  constexpr bool operator!=(const delegate& other) const noexcept
  {
    return (obj_ != other.obj_) || (cb_ != other.cb_);
  }
};

delegate< void(void) > a;

void test()
{
  a();
}

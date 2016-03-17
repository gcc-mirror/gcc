// { dg-do compile }

template <typename Function>
struct function_traits
: public function_traits<decltype(&Function::operator())> 
{ };

template <typename ClassType, typename ReturnType, typename... Args>
struct function_traits<ReturnType(ClassType::*)(Args...) const> 
{
  typedef ReturnType (*pointer)(Args...);
  typedef ReturnType return_type;
};

template <typename Function>
typename function_traits<Function>::pointer
FunctionPointer (const Function& lambda) 
{
  return static_cast<typename function_traits<Function>::pointer>(lambda);
}

template <typename Function>
typename function_traits<Function>::return_type
GetReturnValue (Function func) 
{
  typename function_traits<Function>::return_type *dummy;
  return *dummy;
}
template <typename T> class ClassFoo {};
template <typename T> void FuncBar () { }

template <> inline void FuncBar<double> ()
{
  typedef void (*func_type)(ClassFoo<double> &);
  func_type f1 = FunctionPointer ([](ClassFoo<double> & ref) { });
}

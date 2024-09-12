// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
#if !__has_include(<coroutine>) \
  && __has_include(<experimental/coroutine>) // for __clang__
#include <experimental/coroutine>
namespace std {
  using namespace std::experimental;
}
#else
#include <coroutine>
#endif
#include <cstdio>
#include <typeinfo>
#include <cxxabi.h>  // needed for abi::__cxa_demangle
#include <memory>

std::shared_ptr<char> cppDemangle(const char *abiName)
{
  int status;    
  char *ret = abi::__cxa_demangle(abiName, 0, 0, &status);  

  /* NOTE: must free() the returned char when done with it! */
  std::shared_ptr<char> retval;
  retval.reset( (char *)ret, [](char *mem) { if (mem) free((void*)mem); } );
  return retval;
}

template <typename T>
struct Id{};
struct Task
{
  struct promise_type
  {        
    void return_void() const noexcept {}

    static void is_int (std::string x) {
      if (x != "Id<int>")
	abort() ;
    }
    template <typename ... Args>
    void* operator new(std::size_t len, Args ...args) noexcept
      {
	(is_int (cppDemangle(typeid(Id<Args>).name()).get()), ...);
	(std::puts (cppDemangle(typeid(Id<Args>).name()).get()), ...);
	return nullptr;
      }

        static Task get_return_object_on_allocation_failure() noexcept
        {
            return {};
        }

        Task get_return_object() noexcept
        {
            return Task{ *this };
        }

        std::suspend_always initial_suspend() noexcept
        {
            return {};
        }

        std::suspend_always final_suspend() noexcept
        {
            return {};
        }

        void unhandled_exception() noexcept {}
    };

    using promise_handle = std::coroutine_handle<promise_type>;

    Task() = default;
    Task(promise_type & promise) noexcept
        : m_handle{ promise_handle::from_promise(promise) }
    {}

    ~Task()
    {
        if (m_handle.address()) { m_handle.destroy(); }
    }
    
    promise_handle m_handle{};
};


Task Foo(auto && ... args) noexcept
{
    co_return;
}

int main()
{
    int v;
    Foo(v, 2134);
}

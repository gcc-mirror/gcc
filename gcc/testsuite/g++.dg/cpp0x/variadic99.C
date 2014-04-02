// PR c++/43054
// { dg-do compile { target c++11 } }

template<typename R> struct future { };

template<typename Fn, typename... Args>
 auto
 async(Fn&& fn, Args&&... args)
 -> future<decltype(fn(args...))>;

template<typename Fn, typename... Args>
 auto
 async(Fn&& fn, Args&&... args)
 -> future<decltype(fn(args...))>;

int work2(int value);

void work(int value)
{
  async(work2, value);
}


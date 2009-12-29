// PR c++/42447

template<int>
  void* get(int);

template<typename>
  struct unique_ptr;

template<typename _Tp>
  struct unique_ptr<_Tp[]>
  {
    typedef int __tuple_type;

    void*
    get() const
    { return ::get<0>(_M_t); }

    __tuple_type _M_t;
  };

template <typename T> class dynamic_dispatch;

template <typename TC>
  struct dynamic_dispatch<void (TC::*)(int&)>
  {
    struct entry { };
    unique_ptr<entry[]> m_Start;

    template <typename UC>
      void attach_handler(void (UC::*m)(int&))
      {
        entry* p = 0;
        do {
        } while(--p != m_Start.get());
      }
  };

template <typename TC>
  class request_dispatcher
  : private dynamic_dispatch<void (TC::*)(int&)>
  { request_dispatcher(); };

struct file_reader
{
  void execute_command(int&);
};

template <>
  request_dispatcher<file_reader>::request_dispatcher()
  { this->attach_handler(&file_reader::execute_command); }

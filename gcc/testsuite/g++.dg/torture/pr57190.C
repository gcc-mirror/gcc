// { dg-do compile }

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     {
    };
}
namespace std {
    template<typename>     class allocator;
    template<class _CharT>     struct char_traits;
    template<typename _CharT, typename _Traits = char_traits<_CharT>,            typename _Alloc = allocator<_CharT> >     class basic_string;
    typedef basic_string<char> string;
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    };
    template<typename _CharT, typename _Traits, typename _Alloc>     class basic_string     {
    public:
	basic_string(const _CharT* __s, const _Alloc& __a = _Alloc());
    };
}
class UIException {
};
class PasswordDialog {
    void run()
#if __cplusplus <= 201402L
    throw (UIException)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
};
class MessageBox  {
public:
    MessageBox (std::string t)
#if __cplusplus <= 201402L
    throw (UIException)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
    virtual int run()
#if __cplusplus <= 201402L
    throw (UIException)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
};
extern "C" {
    struct __jmp_buf_tag   {
    };
    extern int __sigsetjmp (struct __jmp_buf_tag __env[1], int __savemask) throw ();
    typedef struct __jmp_buf_tag sigjmp_buf[1];
}
sigjmp_buf password_dialog_sig_jmp_buf;
void PasswordDialog::run()
#if __cplusplus <= 201402L
throw (UIException)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  __sigsetjmp (password_dialog_sig_jmp_buf, 1);
  MessageBox* errmsg = __null;
  errmsg = new MessageBox ("E R R O R");
  errmsg->run();
}

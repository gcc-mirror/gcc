// { dg-do compile }

namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Iterator>     struct iterator_traits     {
    };
    template<typename _Tp>     struct iterator_traits<_Tp*>     {
	typedef _Tp& reference;
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    using std::iterator_traits;
    template<typename _Iterator, typename _Container>     class __normal_iterator     {
	_Iterator _M_current;
	typedef iterator_traits<_Iterator> __traits_type;
    public:
	typedef typename __traits_type::reference reference;
	explicit       __normal_iterator(const _Iterator& __i) : _M_current(__i) {
	}
	reference       operator*() const       {
	    return *_M_current;
	}
	__normal_iterator       operator++(int)       {
	    return __normal_iterator(_M_current++);
	}
    };
    template<typename _Tp>     class new_allocator     {
    public:
	typedef _Tp* pointer;
	template<typename _Tp1>         struct rebind         {
	    typedef new_allocator<_Tp1> other;
	};
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Alloc>   struct __alloc_traits   {
	typedef typename _Alloc::pointer pointer;
	template<typename _Tp>       struct rebind       {
	    typedef typename _Alloc::template rebind<_Tp>::other other;
	};
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp, typename _Alloc>     struct _Vector_base     {
	typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template         rebind<_Tp>::other _Tp_alloc_type;
	typedef typename __gnu_cxx::__alloc_traits<_Tp_alloc_type>::pointer         pointer;
	struct _Vector_impl       : public _Tp_alloc_type       {
	    pointer _M_start;
	};
	_Vector_impl _M_impl;
    };
    template<typename _Tp, typename _Alloc = std::allocator<_Tp> >     class vector : protected _Vector_base<_Tp, _Alloc>     {
	typedef _Vector_base<_Tp, _Alloc> _Base;
    public:
	typedef typename _Base::pointer pointer;
	typedef __gnu_cxx::__normal_iterator<pointer, vector> iterator;
	iterator       begin()       {
	    return iterator(this->_M_impl._M_start);
	}
    };
}
class myServer {
    static std::vector<myServer *> server_list;
    class Callback;
    class myFolder *currentFolder;
    static bool eventloop(Callback *);
};
extern "C" {
    typedef unsigned int uint32_t;
    typedef uint32_t unicode_char;
    extern int strcmp (__const char *__s1, __const char *__s2)      throw () __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
};
class CursesObj {
};
class Curses : public CursesObj {
public:
    class Key {
    public:
	unicode_char ukey;
	const char *keycode;
	Key(unicode_char ch) : ukey(ch), keycode(0) {
	}
	bool plain() const {
	}
	bool nokey() const {
	}
	bool operator==(const Key &k) const   {
	    return strcmp(keycode ? keycode:
			  "",           k.keycode ? k.keycode:
			  "") == 0 &&     ukey == k.ukey;
	}
    };
    static bool processKey(const Key &k);
};
class CursesContainer : public Curses {
};
class myFolder {
public:
    void checkExpunged();
};
class Typeahead {
public:
    static Typeahead *typeahead;
    bool empty()  {
    }
    Curses::Key pop()  {
    }
};
class CursesScreen : public CursesContainer {
public:
    Key getKey();
};
using namespace std;
extern CursesScreen *cursesScreen;
bool myServer::eventloop(myServer::Callback *callback) {
    Curses::Key k1=    (callback == __null && !Typeahead::typeahead->empty()     ? Typeahead::typeahead->pop()     : cursesScreen->getKey());
    if (callback == __null || (k1.plain() && k1.ukey == '\x03'))   {
	if (!k1.nokey())    {
	    bool rc=Curses::processKey(k1);
	    if (rc)     {      while (k1.plain() && k1 == '\x03' &&             !Typeahead::typeahead->empty())       Typeahead::typeahead->pop();     }
	}
    }
    vector<myServer *>::iterator b=server_list.begin();
    while (1)   {
	myServer *p= *b++;
	if (p->currentFolder)     p->currentFolder->checkExpunged();
    }
}

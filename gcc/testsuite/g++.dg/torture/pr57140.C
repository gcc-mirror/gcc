// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

namespace std {
    typedef long unsigned int size_t;
    template<typename>     class allocator;
    template<class _CharT>     struct char_traits;
    template<typename _CharT, typename _Traits = char_traits<_CharT>,            typename _Alloc = allocator<_CharT> >     class basic_string;
    typedef basic_string<char> string;
}
namespace std __attribute__ ((__visibility__ ("default"))) {
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     {
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    public:
	template<typename _Tp1>         struct rebind         {
	    typedef allocator<_Tp1> other;
	};
    };
}
namespace std {
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _CharT, typename _Traits, typename _Alloc>     class basic_string     {
	struct _Alloc_hider : _Alloc       {
	    _Alloc_hider(_CharT* __dat, const _Alloc& __a)  : _Alloc(__a), _M_p(__dat) {
	    }
	    _CharT* _M_p;
	};
	mutable _Alloc_hider _M_dataplus;
    public:
	basic_string(const _CharT* __s, const _Alloc& __a = _Alloc());
    };
    template<typename _CharT, typename _Traits, typename _Alloc>     inline bool     operator==(const basic_string<_CharT, _Traits, _Alloc>& __lhs,         const _CharT* __rhs)     {
    }
}
extern "C" {
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    namespace __detail   {
	struct _List_node_base     {
	    _List_node_base* _M_next;
	};
    }
    template<typename _Tp>     struct _List_node : public __detail::_List_node_base     {
    };
    template<typename _Tp>     struct _List_iterator     {
	typedef _List_iterator<_Tp> _Self;
	typedef _Tp& reference;
	reference       operator*() const       {
	}
	bool       operator!=(const _Self& __x) const       {
	}
    };
    template<typename _Tp, typename _Alloc>     class _List_base     {
    protected:
	typedef typename _Alloc::template rebind<_List_node<_Tp> >::other         _Node_alloc_type;
	struct _List_impl       : public _Node_alloc_type       {
	    __detail::_List_node_base _M_node;
	    _List_impl()  : _Node_alloc_type(), _M_node()  {
	    }
	    _List_impl(const _Node_alloc_type& __a)  : _Node_alloc_type(__a), _M_node()  {
	    }
	};
	_List_impl _M_impl;
	~_List_base()       {
	}
	void       _M_clear();
    };
    template<typename _Tp, typename _Alloc = std::allocator<_Tp> >     class list : protected _List_base<_Tp, _Alloc>     {
	typedef _List_iterator<_Tp> iterator;
	typedef size_t size_type;
    public:
	iterator       begin()       {
	}
	iterator       end()       {
	}
	bool       empty() const       {
	}
	size_type       size() const       {
	}
	void       swap(list& __x)       {
	}
	template<typename _StrictWeakOrdering>         void         merge(list& __x, _StrictWeakOrdering __comp);
	template<typename _StrictWeakOrdering>         void         sort(_StrictWeakOrdering);
    };
    template<typename _Tp, typename _Alloc>     template <typename _StrictWeakOrdering>       void       list<_Tp, _Alloc>::       merge(list& __x, _StrictWeakOrdering __comp)       {
	if (this != &__x)    {
	    iterator __first1 = begin();
	    iterator __last1 = end();
	    iterator __first2 = __x.begin();
	    iterator __last2 = __x.end();
	    while (__first1 != __last1 && __first2 != __last2)        if (__comp(*__first2, *__first1))   {
		iterator __next = __first2;
		__first2 = __next;
	    }
	}
    }
    template<typename _Tp, typename _Alloc>     template <typename _StrictWeakOrdering>       void       list<_Tp, _Alloc>::       sort(_StrictWeakOrdering __comp)       {
	if (this->_M_impl._M_node._M_next != &this->_M_impl._M_node      && this->_M_impl._M_node._M_next->_M_next != &this->_M_impl._M_node)    {
	    list __carry;
	    list __tmp[64];
	    list * __fill = &__tmp[0];
	    list * __counter;
	    do        {
		for(__counter = &__tmp[0];
		    __counter != __fill && !__counter->empty();
		    ++__counter)     {       __counter->merge(__carry, __comp);       __carry.swap(*__counter);     }
	    }
	    while ( !empty() );
	    for (__counter = &__tmp[1];
		 __counter != __fill;
		 ++__counter)        __counter->merge(*(__counter - 1), __comp);
	}
    }
}
namespace gloox {
    class Tag   {
    };
    class StanzaExtension   {
    };
}
namespace gloox {
}
using namespace gloox;
class AbstractPurpleRequest {
};
class AdhocCommandHandler : public AbstractPurpleRequest {
};
class AdhocTag : public Tag {
};
class AbstractConfigInterfaceHandler {
};
namespace gloox {
    class DataFormField   {
    public:
	const std::string& value() const {
	}
    };
    class DataFormFieldContainer   {
    public:
	bool hasField( const std::string& field ) const         {
	}
	DataFormField* field( const std::string& field ) const;
    };
    class DataForm : public StanzaExtension, public DataFormFieldContainer   {
    };
}
enum {
    SORT_BY_JID,  SORT_BY_UIN,  SORT_BY_BUDDIES, };
struct SortData {
};
struct ListData {
    std::list<SortData> output;
    int sort_by;
};
class AdhocAdmin : public AdhocCommandHandler, public AbstractConfigInterfaceHandler {
    AdhocTag *handleAdhocTag(Tag *stanzaTag);
    AdhocTag *handleUnregisterUserForm(Tag *tag, const DataForm &form);
    AdhocTag *handleListUsersForm(Tag *tag, const DataForm &form);
    ListData m_listUsersData;
};
namespace gloox {
}
static bool compareIDataASC(SortData &a, SortData &b) {
}
AdhocTag *AdhocAdmin::handleListUsersForm(Tag *tag, const DataForm &form) {
    ListData &data = m_listUsersData;
    if (data.output.size() == 0) {
	if (!form.hasField("users_vip") || !form.hasField("show_jid") || !form.hasField("show_uin")    || !form.hasField("show_buddies") || !form.hasField("show_sort_by") || !form.hasField("show_sort_order")    || !form.hasField("show_max_per_page")   ) {
	}
	bool sort_asc = form.field("show_sort_order")->value() == "asc";
	if (data.sort_by == SORT_BY_BUDDIES) {
	    if (sort_asc)     data.output.sort(compareIDataASC);
	}
	else {
	}
    }
}

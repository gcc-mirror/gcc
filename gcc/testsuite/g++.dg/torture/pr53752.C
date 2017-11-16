// { dg-do compile }
// { dg-options "-g" }
// { dg-additional-options "-Wno-return-type" }

typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;
namespace mpl_ {
    template< typename T, T N > struct integral_c {
	static const T value = N;
    };
}
namespace mpl {
    using namespace mpl_;
};
template <class T, T val> struct integral_constant : public mpl::integral_c<T, val> {
    typedef integral_constant<bool,false> type;
};
template< typename T > struct is_lvalue_reference : public ::integral_constant<bool,false> {
};
template< typename T > struct is_rvalue_reference : public ::integral_constant<bool,false> {
};
namespace type_traits {
    template <bool b1, bool b2, bool b3 = false, bool b4 = false, bool b5 = false, bool b6 = false, bool b7 = false> struct ice_or;
    template <> struct ice_or<false, false, false, false, false, false, false> {
	static const bool value = false;
    };
}
template <typename T> struct is_reference_impl {
    static const bool value = (::type_traits::ice_or< ::is_lvalue_reference<T>::value, ::is_rvalue_reference<T>::value >::value)                  ;
};
template< typename T > struct is_reference : public ::integral_constant<bool,::is_reference_impl<T>::value> {
};
struct na {
};
namespace mpl {
    template<       bool C     , typename T1     , typename T2     > struct if_c {
	typedef T2 type;
    };
    template<       typename T1 = na     , typename T2 = na     , typename T3 = na     > struct if_ {
	typedef if_c<           static_cast<bool>(T1::value)         , T2         , T3         > almost_type_;
	typedef typename almost_type_::type type;
    };
}
namespace optional_detail {
    template<class T> struct types_when_isnt_ref {
	typedef T & reference_type ;
    }
    ;
    template<class T> struct types_when_is_ref {
    }
    ;
    struct optional_tag {
    }
    ;
    template<class T> class optional_base : public optional_tag {
	typedef types_when_isnt_ref<T> types_when_not_ref ;
	typedef types_when_is_ref<T> types_when_ref ;
    protected :
	typedef typename is_reference<T>::type is_reference_predicate ;
	typedef typename mpl::if_<is_reference_predicate,types_when_ref,types_when_not_ref>::type types ;
	typedef typename types::reference_type reference_type ;
    }
    ;
}
template<class T> class optional : public optional_detail::optional_base<T> {
    typedef optional_detail::optional_base<T> base ;
public :
    typedef typename base::reference_type reference_type ;
    reference_type operator *() {
    }
};
namespace noncopyable_ {
    class noncopyable   {
    };
}
typedef noncopyable_::noncopyable noncopyable;
template<class T> class shared_ptr {
public:
    T * operator-> () const     {
    }
};
typedef uint64_t block_address;
class transaction_manager : noncopyable {
public:
    typedef shared_ptr<transaction_manager> ptr;
};
template <typename ValueType>  class NoOpRefCounter {
};
struct uint64_traits {
};
namespace btree_detail {
    class shadow_spine : private noncopyable {
    public:
	shadow_spine(transaction_manager::ptr tm)     : tm_(tm) {
	}
	transaction_manager::ptr tm_;
    };
}
template <unsigned Levels, typename ValueTraits>  class btree {
public:
    typedef shared_ptr<btree<Levels, ValueTraits> > ptr;
    typedef uint64_t key[Levels];
    typedef typename ValueTraits::value_type value_type;
    typedef optional<value_type> maybe_value;
    btree(typename transaction_manager::ptr tm,         typename ValueTraits::ref_counter rc);
    maybe_value lookup(key const &key) const;
    void insert(key const &key, typename ValueTraits::value_type const &value);
    template <typename ValueTraits2>   bool   insert_location(btree_detail::shadow_spine &spine,     block_address block,     uint64_t key,     int *index);
    typename transaction_manager::ptr tm_;
    block_address root_;
    typename ValueTraits::ref_counter rc_;
};
template <unsigned Levels, typename ValueTraits> void btree<Levels, ValueTraits>:: insert(key const &key,        typename ValueTraits::value_type const &value) {
    using namespace btree_detail;
    block_address block = root_;
    int index = 0;
    shadow_spine spine(tm_);
    for (unsigned level = 0;
	 level < Levels - 1;
	 ++level) {
	bool need_insert = insert_location<uint64_traits>(spine, block, key[level], &index);
	if (need_insert) {
	    btree<Levels - 1, ValueTraits> new_tree(tm_, rc_);
	}
    }
}
struct device_details_disk {
}
__attribute__ ((packed));
struct device_details {
};
struct device_details_traits {
    typedef device_details value_type;
    typedef NoOpRefCounter<device_details> ref_counter;
};
typedef uint32_t thin_dev_t;
typedef btree<1, device_details_traits> detail_tree;
struct metadata {
    typedef shared_ptr<metadata> ptr;
    detail_tree::ptr details_;
};
class thin_pool;
class thin {
    void set_snapshot_time(uint32_t time);
    thin_dev_t dev_;
    thin_pool *pool_;
};
class thin_pool {
public:
    metadata::ptr md_;
};
void thin::set_snapshot_time(uint32_t time) {
    uint64_t key[1] = {
	dev_ };
    optional<device_details> mdetail = pool_->md_->details_->lookup(key);
    pool_->md_->details_->insert(key, *mdetail);
}

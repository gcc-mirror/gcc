// { dg-do compile }

typedef __SIZE_TYPE__ size_t;
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     {
    public:
	typedef size_t size_type;
	typedef _Tp* pointer;
	typedef _Tp& reference;
	void       deallocate(pointer __p, size_type)       {
	    ::operator delete(__p);
	}
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    public:
	template<typename _Tp1>         struct rebind         {
	    typedef allocator<_Tp1> other;
	};
    };
    template<typename _Tp, typename _Alloc>     struct _Vector_base     {
	typedef typename _Alloc::template rebind<_Tp>::other _Tp_alloc_type;
	struct _Vector_impl       : public _Tp_alloc_type       {
	    typename _Tp_alloc_type::pointer _M_start;
	    typename _Tp_alloc_type::pointer _M_end_of_storage;
	};
	~_Vector_base()       {
	    _M_deallocate(this->_M_impl._M_start, this->_M_impl._M_end_of_storage         - this->_M_impl._M_start);
	}
	_Vector_impl _M_impl;
	void       _M_deallocate(typename _Tp_alloc_type::pointer __p, size_t __n)       {
	    if (__p)    _M_impl.deallocate(__p, __n);
	}
    };
    template<typename _Tp, typename _Alloc = std::allocator<_Tp> >     class vector : protected _Vector_base<_Tp, _Alloc>     {
	typedef _Vector_base<_Tp, _Alloc> _Base;
	typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
    public:
	typedef typename _Tp_alloc_type::reference reference;
	typedef size_t size_type;
	size_type       size() const       {
	}
	reference       operator[](size_type __n)       {
	}
    };
};
class vtkConvexPointSet  {
public:
    static vtkConvexPointSet *New();
};
void MakeInternalMesh() {
    std::vector< int > tempFaces[2];
    std::vector< int > firstFace;
    int i, j, k;
    for(i = 0; i < 1000; i++)     {
	for(int pointCount = 0; pointCount < 1000; pointCount++) 	{
	    for(j = 0; j < (int)tempFaces[0].size(); k++)
	      if(tempFaces[0][j] == tempFaces[1][k]) 		break;
	}
	vtkConvexPointSet::New();
    }
}

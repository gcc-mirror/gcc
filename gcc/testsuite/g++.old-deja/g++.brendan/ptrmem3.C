// { dg-do assemble  }
// GROUPS passed pointers-to-members
template<class T> class TemplA {
    T t;
};


template<class T> class TemplB {
public:    
    typedef void (T::*TClassMethod)();

private:
/*
   This line should not crash cuz of the get_decl_list change in this:
	* cp-tree.c (list_hash_lookup_or_cons): Make sure the type doesn't
	have TYPE_PTRMEMFUNC_P set before we try to build its
	CLASSTYPE_ID_AS_LIST.
	(get_decl_list): Likewise, when trying to read it.
*/
    TemplA<TClassMethod> Tmethod;  
};


class C {
    int a;
};

int main(int, char**) {

    TemplB<C> test;
}


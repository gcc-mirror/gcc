// { dg-do compile }

// Origin: Richard Guenther <rguenth@tat.physik.uni-tuebingen.de>

// PR c++/9432: ICE in validate_nonmember_using_decl when decl is a 
// SCOPE_REF.

template <class T> struct Foo;
template <class T>
struct Bar : public Foo<T> {
        void foo()
        {
                using Foo<T>::i;	// { dg-error "member at non-class scope" }
        }
};

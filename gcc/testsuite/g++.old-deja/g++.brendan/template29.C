// Build don't link: 
// GROUPS passed templates
   template <class ElementType> class A
    { public:
       A(ElementType) {}
       ElementType get() const ;
    };

    template <class ElementType> ElementType A<ElementType>::get() const
    { return ElementType(0); }

int main() { const A<short> a(3); }

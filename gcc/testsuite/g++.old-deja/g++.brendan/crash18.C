// { dg-do compile }
// GROUPS passed old-abort
typedef int element;
class Pix {
public:
    Pix();
    Pix(const Pix&);

    // Friend functions so that v == x works as does x == v works
    friend int operator==(void *v, const Pix& x) // { dg-error "previously" }
    { return v == index; }  // { dg-error "from this location" }
    // ??? should be operator!=
    friend int operator==(void *v, const Pix& x) // { dg-error "redefinition" }
    { return v != index; }
private:
//    friend class List<T>;
    element *index; // { dg-error "invalid use of non-static data member" }
};

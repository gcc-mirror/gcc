// { dg-do assemble  }
// GROUPS passed old-abort
typedef int element;
class Pix {
public:
    Pix();
    Pix(const Pix&);

    // Friend functions so that v == x works as does x == v works
    friend int operator==(void *v, const Pix& x)
        { return v == index; }// { dg-error "" } .*
    friend int operator==(void *v, const Pix& x)
        { return v != index; }// { dg-error "" } .*
private:
//    friend class List<T>;
    element *index; // { dg-error "" } invalid use of member
};

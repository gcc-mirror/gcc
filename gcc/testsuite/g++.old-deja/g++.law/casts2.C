// Build don't link: 
// GROUPS passed casts
class VObject;

typedef int boolean;

typedef boolean (VObject::*method)();
typedef boolean (VObject::*method0)();
typedef boolean (VObject::*method1)(long);

#define methodOf(o,m)  (method)(&o::m)


class VObject {
    public:
    boolean perform(method );
    boolean perform(method , long);
    void    affectMethod(method );
    void    dummy(){};
};


boolean VObject::perform(method m)
{
        method0 q = (method0)m;
        return(this->*q)();
}


boolean VObject::perform(method m, long param)
{
        method1 q = (method1)m;
        return(this->*q)(param);
 }

void VObject::affectMethod(method m)
{
        m =   methodOf(VObject, dummy);
}

// { dg-do assemble  }
class A
{
public:
    A (const A& ccref);
    friend A const re (const A& v1); // { dg-error "" } 
};

A // const
re (const A& ref)
{				// { dg-error "" } mismatched decls
    return A (ref);
}

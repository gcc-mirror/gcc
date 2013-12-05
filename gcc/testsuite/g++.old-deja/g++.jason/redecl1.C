// { dg-do assemble  }
class A
{
public:
    A (const A& ccref);
    friend A const re (const A& v1); // { dg-message "old declaration" } 
};

A // const
re (const A& ref) // { dg-error "new declaration" }
{
    return A (ref);
}

class A
{
public:
    A (const A& ccref);
    friend A const re (const A& v1); // ERROR - 
};

A // const
re (const A& ref)
{				// ERROR - mismatched decls
    return A (ref);
}

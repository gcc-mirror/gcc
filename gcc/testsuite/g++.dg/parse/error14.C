// { dg-options "-fshow-column" }
// PR c++/13970

struct X
{
    template< typename Z > Z Zunc()
    {
        return Z();
    }

    template< typename Z > void Zinc()
    {
    }

    void tst()
    {
        Zunc<int>();

        Zinc<int>( //);
		  //    }

}; // { dg-error "2: error: expected '.' at end of input|1: error: expected primary-expression before '.' token|1: error: expected ';' before '.' token|1: error: expected unqualified-id at end of input" }


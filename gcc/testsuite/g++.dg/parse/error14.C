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

}; // { dg-error "" }


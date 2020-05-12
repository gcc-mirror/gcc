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

};   // { dg-error "1:expected primary-expression before '.' token" "primary"  }
// { dg-error "-:expected" "at end of input" { target *-*-* } .+1 }

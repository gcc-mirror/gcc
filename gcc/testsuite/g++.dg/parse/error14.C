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

}; // { dg-error "2:expected '.' at end of input" "at end of input" }
   // { dg-error "1:expected primary-expression before '.' token" "primary" { target *-*-* } 22 }
   // { dg-error "1:expected unqualified-id" "unqualified-id" { target *-*-* } 22 }
   // { dg-error "1:expected ';' before '.' token" "function" { target *-*-* } 22 }


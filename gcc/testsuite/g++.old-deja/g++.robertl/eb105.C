// { dg-do assemble  }
template< class T >
void    sort( T* t, int n )
        {
            struct
/*line5*/   {
                int     operator()(T i, T j)
                        {
                            return (i < j) ? -1 : ((j < i) ? 1 : 0) ;
                        }
            } c ;
            sort(t, n, c, 0) ;
        }

int gfbyte ( void ) 
{
 return 0;
} 

int main( void ) 
{
 int i,j,k ;

 i = gfbyte();

 i = i + 1 ;

 if ( i == 0 ) 
     k = -0 ;
 else
     k = i + 0 ;

 if (i != 1)
   abort ();

 k = 1 ;
 if ( k <= i)
     do 
	 j = gfbyte () ;
     while ( k++ < i ) ;

 exit (0);
} 


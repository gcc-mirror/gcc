typedef union {
    struct {
	long RH, LH;
    } v;
    struct {
	unsigned char B1;
    } u;
} twohalves;


typedef union {
    long cint;
    twohalves hhfield;

} memoryword;


extern  memoryword mem[262];

long znewstructure ( p )
long p;
{
  long q, r;
    {
      q = mem [ p + 2 ] .hhfield .v.RH;
      r = mem [ q + 1 ] .hhfield .v.LH;
      do {
	  q = r;
	r = mem [ r ] .hhfield .v.RH;
      } while ( ! ( r == p ) );
      r = foo((long) ( 3 ));
      mem [ q ] .hhfield .v.RH = r;
      mem [ r + 2 ] = mem [ p + 2 ];
      if ( mem [ p + 2 ] .hhfield .v.LH  == 0 )
      {
	q = mem [ p + 2 ] .hhfield .v.RH + 1;
	while ( mem [ q ] .hhfield .v.RH != p ) q = mem [ q ] .hhfield .v.RH;
	mem [ q ] .hhfield .v.RH = r;
      }
    }
  mem [ r ] .hhfield .u.B1  = mem [ p ] .hhfield .u.B1;
  mem [ r + 1 ] .hhfield .v.LH  = p;

  q = foo((long) ( 3 ));

  mem [ r + 1 ] .hhfield .v.RH = q;
  mem [ q + 2 ] .hhfield .v.RH = r;


  return(r);
}

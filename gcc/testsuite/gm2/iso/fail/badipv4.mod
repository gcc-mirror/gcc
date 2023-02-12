MODULE badipv4 ;

TYPE
  IPV4 = ARRAY [1..4] OF CHAR ;

CONST
  Loopback = IPV4 {127, 0, 0, 1} ;

END badipv4.
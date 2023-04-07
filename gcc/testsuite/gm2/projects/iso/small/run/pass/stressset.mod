IMPLEMENTATION MODULE stressset ;

TYPE
   enum = (red, blue, green) ;

CONST
   (* max = ORD (MAX (enum)) + 1 ; *)
   max = MAX (enum) + 1 ;


TYPE
   dataType = POINTER TO RECORD
                            next    : dataType ;
			    contents: ARRAY [0..max] OF CARDINAL ;
			    set     : SET OF enum ;
                         END ;

END stressset.
MODULE subrangecase4 ;  (*!m2iso+gm2*)


TYPE
   DateRange = [1910..1920] ;


PROCEDURE init (d: DateRange) ;
BEGIN
   CASE d OF

   1910: |
   1913..1918: |

   END
END init ;


VAR
   year: DateRange ;
BEGIN
   init (year)
END subrangecase4.

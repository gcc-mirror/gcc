MODULE subrangecase2 ;  (*!m2iso+gm2*)


TYPE
   DateRange = [1710..1720] ;


PROCEDURE init (d: DateRange) ;
BEGIN
   CASE d OF

   1711..1720: |

   END
END init ;


VAR
   year: DateRange ;
BEGIN
   init (year)
END subrangecase2.

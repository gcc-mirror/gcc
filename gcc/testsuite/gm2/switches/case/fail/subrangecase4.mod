MODULE subrangecase4 ;  (*!m2iso+gm2*)


TYPE
   DateRange = [1710..1720] ;


PROCEDURE init (d: DateRange) ;
BEGIN
   CASE d OF

   1710: |
   1713..1718: |

   END
END init ;


VAR
   year: DateRange ;
BEGIN
   init (year)
END subrangecase4.

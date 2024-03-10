MODULE subrangecase6 ;  (*!m2iso+gm2*)


TYPE
   alphabet = [MIN (CHAR)..MAX (CHAR)] ;


PROCEDURE init (a: alphabet) ;
BEGIN
   CASE a OF

   'a',
   'e'..'x':

   END
END init ;


VAR
   a: alphabet ;
BEGIN
   init (a)
END subrangecase6.

MODULE subrangecase5 ;  (*!m2iso+gm2*)


TYPE
   alphabet = ['a'..'z'] ;


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
END subrangecase5.

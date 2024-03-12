MODULE calendar ;


TYPE
   DayRange = [1..30] ;


PROCEDURE sept1752 (day: DayRange) : BOOLEAN ;
BEGIN
   CASE day OF

   1..2,
   14..30:  RETURN TRUE

   END ;
   RETURN FALSE
END sept1752 ;


BEGIN
   IF sept1752 (4)
   THEN
   END
END calendar.

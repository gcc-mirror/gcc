MODULE testaddress ;

FROM SYSTEM IMPORT ADDRESS ;

VAR
   a: ADDRESS ;
   c: CARDINAL ;
BEGIN
   c := a + 'z'
END testaddress.
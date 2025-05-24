-- C87A05A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT FUNCTION CALLS AND INDEXED COMPONENT EXPRESSIONS CAN BE 
-- DISTINGUISHED BY THE RULES OF OVERLOADING RESOLUTION.
--
-- PART 1 : CORRECT RESOLUTION IS INDEXED COMPONENT EXPRESSION
  
-- TRH  13 JULY 82
-- DSJ  09 JUNE 83
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87A05A IS
     
     OK : BOOLEAN := TRUE;
     TYPE VECTOR IS ARRAY (1 .. 5) OF BOOLEAN;
  
     PROCEDURE P (ARG : BOOLEAN) IS          -- THIS IS CORRECT P
     BEGIN 
          OK := ARG;
     END P;
  
     PROCEDURE P (ARG : CHARACTER) IS
     BEGIN 
          OK := FALSE;
     END P;

     FUNCTION Y RETURN VECTOR IS             -- THIS IS CORRECT Y
     BEGIN
          RETURN (VECTOR'RANGE => TRUE);
     END Y;

     FUNCTION Y (ARG : INTEGER) RETURN FLOAT IS
     BEGIN 
          OK := FALSE;
          RETURN 0.0;
     END Y;
       
     FUNCTION Y (ARG : CHARACTER) RETURN CHARACTER IS
     BEGIN 
          OK := FALSE;
          RETURN 'A';
     END Y;
   
     FUNCTION Y (ARG : FLOAT) RETURN FLOAT IS
     BEGIN 
          OK := FALSE;
          RETURN 0.0;
     END Y;
       
     FUNCTION Y RETURN BOOLEAN IS
     BEGIN 
          OK := FALSE;
          RETURN FALSE;
     END Y;
       
     FUNCTION Y (ARG : CHARACTER := 'A') RETURN BOOLEAN IS
     BEGIN 
          OK := FALSE;
          RETURN FALSE;
     END Y;
    
     FUNCTION Z RETURN INTEGER IS            -- THIS IS CORRECT Z
     BEGIN 
          RETURN 3;
     END Z;
       
     FUNCTION Z RETURN FLOAT IS
     BEGIN 
          OK := FALSE;
          RETURN 3.0;
     END Z;
   
BEGIN
     TEST ("C87A05A","OVERLOADING RESOLUTION FOR DISTINGUISHING " &
           "FUNCTION CALLS FROM INDEXED COMPONENTS WHERE INDEXED " &
           "COMPONENTS ARE CORRECT");
  
     P (Y (Z) );
  
     IF NOT OK THEN 
          FAILED ("RESOLUTION INCORRECT");
     END IF;
    
     RESULT;
END C87A05A;

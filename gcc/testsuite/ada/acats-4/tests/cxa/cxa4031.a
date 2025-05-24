-- CXA4031.A
--
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
--
-- OBJECTIVE:
--      Check that the subprograms defined in package Ada.Strings.Unbounded
--      are available, and that they produce correct results. Specifically, 
--      check the functions To_Unbounded_String (version with Length
--      parameter), "=", "<", "<=", ">", ">=" (all with String-Unbounded
--      String parameter mix), as well as three versions of Procedure Append.
--      
-- TEST DESCRIPTION:
--      This test demonstrates the uses of many of the subprograms defined
--      in package Ada.Strings.Unbounded for use with unbounded strings.
--      The test simulates how unbounded strings could be processed in a
--      user environment, using the subprograms provided in this package.
--
--      This test, when taken in conjunction with tests CXA4010, CXA4011,
--      CXA4030, and CXA4032 will constitute a test of all the functionality 
--      contained in package Ada.Strings.Unbounded.  This test uses a variety 
--      of the subprograms defined in the unbounded string package in ways 
--      typical of common usage.
--      
--       
-- CHANGE HISTORY:
--      27 Feb 95   SAIC    Initial prerelease version.
--      18 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with Report;
with Ada.Exceptions;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

procedure CXA4031 is
begin

   Report.Test ("CXA4031", "Check that the subprograms defined in "        &
                           "package Ada.Strings.Unbounded are available, " &
                           "and that they produce correct results");

   Test_Block:
   declare

      package Unb renames Ada.Strings.Unbounded;
      use Unb;
      use Ada.Exceptions;
 
      subtype LC_Characters is Character range 'a'..'z';

      Null_String       : constant String := "";
      TC_String         : constant String := "A Standard String";

      TC_Unb_String,
      TC_New_Unb_String : Unb.Unbounded_String := Unb.Null_Unbounded_String;

   begin

      -- Function To_Unbounded_String (version with Length parameter)
      -- returns an unbounded string that represents an uninitialized String
      -- whose length is Length.
      -- Note: Unbounded_String length can vary conceptually between 0 and
      --       Natural'Last.

      if Unb.Length(Unb.To_Unbounded_String(Length => 10)) /= 10 or
         Unb.Length(Unb.To_Unbounded_String(1))            /=  1 or
         Unb.Length(Unb.To_Unbounded_String(0))            /=  0 or
         Unb.Length(Unb."&"(Unb.To_Unbounded_String(Length => 10),
                    Unb."&"(Unb.To_Unbounded_String(1),
                            Unb.To_Unbounded_String(0) ))) /= 10+1+0
      then
         Report.Failed
           ("Incorrect results from Function To_Unbounded_String with " &
            "Length parameter");
      end if;


      -- Procedure Append (Unbounded - Unbounded)
      -- Note: For each of the Append procedures, the resulting string 
      --       represented by the Source parameter is given by the 
      --       concatenation of the original value of Source and the value 
      --       of New_Item.

      TC_Unb_String := Unb.To_Unbounded_String("Sample string of length L");
      TC_New_Unb_String := Unb.To_Unbounded_String(" and then some");

      Unb.Append(Source => TC_Unb_String, New_Item => TC_New_Unb_String);
      
      if TC_Unb_String /= 
         Unb.To_Unbounded_String("Sample string of length L and then some")
      then
         Report.Failed("Incorrect results from Procedure Append with " &
                       "unbounded string parameters - 1");
      end if;


      TC_Unb_String := Unb.To_Unbounded_String("Sample string of length L");
      TC_New_Unb_String := Unb.Null_Unbounded_String;

      Unb.Append(TC_Unb_String, TC_New_Unb_String);
      
      if TC_Unb_String /= 
         Unb.To_Unbounded_String("Sample string of length L")
      then
         Report.Failed("Incorrect results from Procedure Append with " &
                       "unbounded string parameters - 2");
      end if;


      TC_Unb_String := Unb.Null_Unbounded_String;

      Unb.Append(TC_Unb_String,
                 Unb.To_Unbounded_String("New Unbounded String"));
      
      if TC_Unb_String /= 
         Unb.To_Unbounded_String("New Unbounded String")
      then
         Report.Failed("Incorrect results from Procedure Append with " &
                       "unbounded string parameters - 3");
      end if;

      
      -- Procedure Append (Unbounded - String)

      TC_Unb_String := Unb.To_Unbounded_String("An Unbounded String and ");

      Unb.Append(Source => TC_Unb_String, New_Item => TC_String);
      
      if TC_Unb_String /= 
         Unb.To_Unbounded_String("An Unbounded String and A Standard String")
      then
         Report.Failed("Incorrect results from Procedure Append with " &
                       "an unbounded string parameter and a string "   &
                       "parameter - 1");
      end if;


      TC_Unb_String := Unb.To_Unbounded_String("An Unbounded String");

      Unb.Append(TC_Unb_String, New_Item => Null_String);
      
      if TC_Unb_String /= 
         Unb.To_Unbounded_String("An Unbounded String")
      then
         Report.Failed("Incorrect results from Procedure Append with " &
                       "an unbounded string parameter and a string "   &
                       "parameter - 2");
      end if;


      TC_Unb_String := Unb.Null_Unbounded_String;

      Unb.Append(TC_Unb_String, TC_String);
      
      if TC_Unb_String /= Unb.To_Unbounded_String("A Standard String") then
         Report.Failed("Incorrect results from Procedure Append with " &
                       "an unbounded string parameter and a string "   &
                       "parameter - 3");
      end if;


      -- Procedure Append (Unbounded - Character)

      TC_Unb_String := Unb.To_Unbounded_String("Lower Case = ");

      for i in LC_Characters'Range loop
         Unb.Append(Source => TC_Unb_String, New_Item => LC_Characters(i));
      end loop;

      if TC_Unb_String /= 
         Unb.To_Unbounded_String("Lower Case = abcdefghijklmnopqrstuvwxyz")
      then
         Report.Failed("Incorrect results from Procedure Append with "  &
                       "an unbounded string parameter and a character " &
                       "parameter - 1");
      end if;        

      
      TC_Unb_String := Unb.Null_Unbounded_String;

      Unb.Append(TC_Unb_String, New_Item => 'a');
      
      if TC_Unb_String /= Unb.To_Unbounded_String("a") then
         Report.Failed("Incorrect results from Procedure Append with "  &
                       "an unbounded string parameter and a character " &
                       "parameter - 2");
      end if;        


      -- Function "="

      TC_Unb_String := Unb.To_Unbounded_String(TC_String);

      if not (TC_Unb_String = TC_String)                 or  -- (Unb_Str, Str)
         not Unb."="("A Standard String", TC_Unb_String) or  -- (Str, Unb_Str)
         not ((Unb.Null_Unbounded_String = "") and           -- (Unb_Str, Str)
              ("Test String" =                               -- (Str, Unb_Str)
               Unb.To_Unbounded_String("Test String")))
      then
         Report.Failed("Incorrect results from function ""="" with " &
                       "string - unbounded string parameter combinations");
      end if;


      -- Function "<"

      if not ("Extra Space" < Unb.To_Unbounded_String("Extra Space ") and
              Unb.To_Unbounded_String("tess") < "test"                and
              Unb.To_Unbounded_String("best") < "test")                or
         Unb.Null_Unbounded_String            < Null_String            or
         " leading blank"  < Unb.To_Unbounded_String(" leading blank") or
         "ending blank "   < Unb.To_Unbounded_String("ending blank ")  
      then
         Report.Failed("Incorrect results from function ""<"" with " &
                       "string - unbounded string parameter combinations");
      end if;


      -- Function "<="

      TC_Unb_String := Unb.To_Unbounded_String("Sample string");

      if TC_Unb_String                 <= "Sample strin" or  -- (Unb_Str, Str)
         "sample string"               <= TC_Unb_String  or  -- (Str, Unb_Str)
         not(Unb.Null_Unbounded_String <= "")            or  -- (Unb_Str, Str)
         not("Sample string"           <= TC_Unb_String)     -- (Str, Unb_Str)
      then
         Report.Failed("Incorrect results from function ""<="" with " &
                       "string - unbounded string parameter combinations");
      end if;


      -- Function ">"

      TC_Unb_String := Unb.To_Unbounded_String("A MUCH LONGER STRING");

      if not ("A much longer string" > TC_Unb_String                  and 
              Unb.To_Unbounded_String(TC_String) > "A Standard Strin" and
              "abcdefgh" > Unb.To_Unbounded_String("ABCDEFGH"))        or
         Unb.Null_Unbounded_String > Null_String    
      then
         Report.Failed("Incorrect results from function "">"" with " &
                       "string - unbounded string parameter combinations");
      end if;


      -- Function ">=" 

      TC_Unb_String := Unb.To_Unbounded_String(TC_String);

      if not (TC_Unb_String >= TC_String                       and 
              Null_String   >= Unb.Null_Unbounded_String       and
              "test"        >= Unb.To_Unbounded_String("tess") and  
              Unb.To_Unbounded_String("Programming") >= "PROGRAMMING")
      then
         Report.Failed("Incorrect results from function "">="" with " &
                       "string - unbounded string parameter combinations");
      end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXA4031;

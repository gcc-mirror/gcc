-- CB20A02.A
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
--      Check that the name and pertinent information about a user defined
--      exception are available to an enclosing program unit even when the 
--      enclosing unit has no visibility into the scope where the exception
--      is declared and raised.
--
-- TEST DESCRIPTION:
--      Declare a subprogram nested within the test subprogram.  The enclosing 
--      subprogram does not have visibility into the nested subprogram.
--      Declare and raise an exception in the nested subprogram, and allow
--      the exception to propagate to the enclosing scope.  Use the function
--      Exception_Name in the enclosing subprogram to produce exception 
--      specific information when the exception is handled in an others 
--      handler.
--
-- TEST FILES:
--
--      This test depends on the following foundation code file:
--         FB20A00.A
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with FB20A00;             -- Package containing Function Find
with Ada.Exceptions;
with Report;

procedure CB20A02 is

   Seed_Number   : Integer;
   Random_Number : Integer := 0;

     --=================================================================--

   function Random_Number_Generator (Seed : Integer) return Integer is

      Result : Integer := 0;

      HighSeedError,
      Mid_Seed_Error,
      L_o_w_S_e_e_d_E_r_r_o_r : exception;

   begin  -- Random_Number_Generator


      if (Report.Ident_Int (Seed)    > 1000) then
         raise HighSeedError;
      elsif (Report.Ident_Int (Seed) >  100) then
         raise Mid_Seed_Error;
      elsif (Report.Ident_Int (Seed) >   10) then
         raise L_o_w_S_e_e_d_E_r_r_o_r;
      else
         Seed_Number := ((Seed_Number * 417) + 231) mod 53;
         Result := Seed_Number / 52;
      end if;

      return Result;

   end Random_Number_Generator;

     --=================================================================--

begin

   Report.Test ("CB20A02", "Check that the name "                        &
                           "of a user defined exception is available "   &
                           "to an enclosing program unit even when the " &
                           "enclosing unit has no visibility into the "  &
                           "scope where the exception is declared and "  &
                           "raised" );

   High_Seed:
   begin
      -- This seed value will result in the raising of a HighSeedError
      -- exception.
      Seed_Number   := 1001;
      Random_Number := Random_Number_Generator (Seed_Number);
      Report.Failed ("Exception not raised in High_Seed block");
   exception
      when Error : others =>
         if not FB20A00.Find (Ada.Exceptions.Exception_Name (Error),
                              "HighSeedError")
         then
            Report.Failed ("Expected HighSeedError, but found " &
                            Ada.Exceptions.Exception_Name (Error));
         end if;
   end High_Seed;


   Mid_Seed:
   begin        
      -- This seed value will generate a Mid_Seed_Error exception.
      Seed_Number   := 101;
      Random_Number := Random_Number_Generator (Seed_Number);
      Report.Failed ("Exception not raised in Mid_Seed block");
   exception
      when Error : others =>
         if not FB20A00.Find (Ada.Exceptions.Exception_Name (Error),
                              "Mid_Seed_Error")
         then
            Report.Failed ("Expected Mid_Seed_Error, but found " &
                            Ada.Exceptions.Exception_Name (Error));
         end if;
   end Mid_Seed;


   Low_Seed:
   begin
      -- This seed value will result in the raising of a
      -- L_o_w_S_e_e_d_E_r_r_o_r exception.
      Seed_Number   := 11;
      Random_Number := Random_Number_Generator (Seed_Number);
      Report.Failed ("Exception not raised in Low_Seed block");
   exception
      when Error : others =>
         if not FB20A00.Find (Ada.Exceptions.Exception_Name (Error),
                              "L_o_w_S_e_e_d_E_r_r_o_r")
         then
            Report.Failed ("Expected L_o_w_S_e_e_d_E_r_r_o_r but found " &
                            Ada.Exceptions.Exception_Name (Error));
         end if;
   end Low_Seed;


   Report.Result;

end CB20A02;

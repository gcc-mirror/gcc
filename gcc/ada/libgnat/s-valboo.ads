------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ B O O L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with System.Val_Util;

package System.Val_Bool
  with SPARK_Mode
is
   pragma Preelaborate;

   function Is_Boolean_Image_Ghost (Str : String) return Boolean is
     (not System.Val_Util.Only_Space_Ghost (Str, Str'First, Str'Last)
        and then
      (declare
         F : constant Positive := System.Val_Util.First_Non_Space_Ghost
           (Str, Str'First, Str'Last);
       begin
         (F <= Str'Last - 3
          and then Str (F)     in 't' | 'T'
          and then Str (F + 1) in 'r' | 'R'
          and then Str (F + 2) in 'u' | 'U'
          and then Str (F + 3) in 'e' | 'E'
          and then
            (if F + 3 < Str'Last then
               System.Val_Util.Only_Space_Ghost (Str, F + 4, Str'Last)))
           or else
         (F <= Str'Last - 4
          and then Str (F)     in 'f' | 'F'
          and then Str (F + 1) in 'a' | 'A'
          and then Str (F + 2) in 'l' | 'L'
          and then Str (F + 3) in 's' | 'S'
          and then Str (F + 4) in 'e' | 'E'
          and then
            (if F + 4 < Str'Last then
               System.Val_Util.Only_Space_Ghost (Str, F + 5, Str'Last)))))
   with
     Ghost;
   --  Ghost function that returns True iff Str is the image of a boolean, that
   --  is "true" or "false" in any capitalization, possibly surounded by space
   --  characters.

   function Value_Boolean (Str : String) return Boolean
   with
     Pre  => Is_Boolean_Image_Ghost (Str),
     Post =>
       Value_Boolean'Result =
         (Str (System.Val_Util.First_Non_Space_Ghost
            (Str, Str'First, Str'Last)) in 't' | 'T');
   --  Computes Boolean'Value (Str)

end System.Val_Bool;

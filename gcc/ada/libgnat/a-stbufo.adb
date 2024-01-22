------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_BUFFERS.FORMATTING                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2024, Free Software Foundation, Inc.       --
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

with Ada.Strings.Text_Buffers.Unbounded;
with Ada.Strings.Text_Buffers.Files;

package body Ada.Strings.Text_Buffers.Formatting is

   use Ada.Strings.Text_Buffers.Files;
   use Ada.Strings.Text_Buffers.Utils;

   procedure Put
     (S : in out Root_Buffer_Type'Class; T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "")
   is
      J : Positive := T'First;
      Used : array (1 .. 9) of Boolean := [others => False];
   begin
      while J <= T'Last loop
         if T (J) = '\' then
            J := J + 1;
            case T (J) is
               when 'n' =>
                  New_Line (S);
               when '\' =>
                  Put_7bit (S, '\');
               when 'i' =>
                  Increase_Indent (S);
               when 'o' =>
                  Decrease_Indent (S);
               when 'I' =>
                  Increase_Indent (S, 1);
               when 'O' =>
                  Decrease_Indent (S, 1);

               when '1' =>
                  Used (1) := True;
                  Put_UTF_8_Lines (S, X1);
               when '2' =>
                  Used (2) := True;
                  Put_UTF_8_Lines (S, X2);
               when '3' =>
                  Used (3) := True;
                  Put_UTF_8_Lines (S, X3);
               when '4' =>
                  Used (4) := True;
                  Put_UTF_8_Lines (S, X4);
               when '5' =>
                  Used (5) := True;
                  Put_UTF_8_Lines (S, X5);
               when '6' =>
                  Used (6) := True;
                  Put_UTF_8_Lines (S, X6);
               when '7' =>
                  Used (7) := True;
                  Put_UTF_8_Lines (S, X7);
               when '8' =>
                  Used (8) := True;
                  Put_UTF_8_Lines (S, X8);
               when '9' =>
                  Used (9) := True;
                  Put_UTF_8_Lines (S, X9);

               when others =>
                  raise Program_Error;
            end case;
         else
            Put_7bit (S, T (J));
         end if;

         J := J + 1;
      end loop;

      if not Used (1) then
         pragma Assert (X1 = "");
      end if;
      if not Used (2) then
         pragma Assert (X2 = "");
      end if;
      if not Used (3) then
         pragma Assert (X3 = "");
      end if;
      if not Used (4) then
         pragma Assert (X4 = "");
      end if;
      if not Used (5) then
         pragma Assert (X5 = "");
      end if;
      if not Used (6) then
         pragma Assert (X6 = "");
      end if;
      if not Used (7) then
         pragma Assert (X7 = "");
      end if;
      if not Used (8) then
         pragma Assert (X8 = "");
      end if;
      if not Used (9) then
         pragma Assert (X9 = "");
      end if;
   end Put;

   function Format
     (T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "")
     return Utils.UTF_8_Lines
   is
      Buffer : Unbounded.Buffer_Type;
   begin
      Put (Buffer, T, X1, X2, X3, X4, X5, X6, X7, X8, X9);
      return Buffer.Get_UTF_8;
   end Format;

   procedure Put
     (T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "") is
      Buffer : File_Buffer := Create_Standard_Output_Buffer;
   begin
      Put (Buffer, T, X1, X2, X3, X4, X5, X6, X7, X8, X9);
   end Put;

   procedure Err
     (T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "") is
      Buffer : File_Buffer := Create_Standard_Error_Buffer;
   begin
      Put (Buffer, T, X1, X2, X3, X4, X5, X6, X7, X8, X9);
   end Err;

end Ada.Strings.Text_Buffers.Formatting;

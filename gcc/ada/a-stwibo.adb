------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--             A D A . S T R I N G S . W I D E _ B O U N D E D              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Maps;   use Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Search;

package body Ada.Strings.Wide_Bounded is

   package body Generic_Bounded_Length is

      ---------
      -- "&" --
      ---------

      function "&"
        (Left  : in Bounded_Wide_String;
         Right : in Bounded_Wide_String)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left.Length;
         Rlen   : constant Length_Range := Right.Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen > Max_Length then
            raise Ada.Strings.Length_Error;
         else
            Result.Length := Nlen;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);
         end if;

         return Result;
      end "&";

      function "&"
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left.Length;

         Nlen   : constant Natural      := Llen + Right'Length;

      begin
         if Nlen > Max_Length then
            raise Ada.Strings.Length_Error;
         else
            Result.Length := Nlen;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1 .. Nlen) := Right;
         end if;
         return Result;
      end "&";

      function "&"
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left'Length;
         Rlen   : constant Length_Range := Right.Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen > Max_Length then
            raise Ada.Strings.Length_Error;
         else
            Result.Length := Nlen;
            Result.Data (1 .. Llen) := Left;
            Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);
         end if;

         return Result;
      end "&";

      function "&"
        (Left  : in Bounded_Wide_String;
         Right : in Wide_Character)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left.Length;

      begin
         if Llen = Max_Length then
            raise Ada.Strings.Length_Error;
         else
            Result.Length := Llen + 1;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Result.Length) := Right;
         end if;

         return Result;
      end "&";

      function "&"
        (Left  : in Wide_Character;
         Right : in Bounded_Wide_String)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Rlen   : Length_Range := Right.Length;

      begin
         if Rlen = Max_Length then
            raise Ada.Strings.Length_Error;
         else
            Result.Length := Rlen + 1;
            Result.Data (1) := Left;
            Result.Data (2 .. Result.Length) := Right.Data (1 .. Rlen);
         end if;

         return Result;
      end "&";

      ---------
      -- "*" --
      ---------

      function "*"
        (Left  : in Natural;
         Right : in Wide_Character)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;

      begin
         if Left > Max_Length then
            raise Ada.Strings.Length_Error;
         else
            Result.Length := Left;

            for J in 1 .. Left loop
               Result.Data (J) := Right;
            end loop;
         end if;

         return Result;
      end "*";

      function "*"
        (Left  : in Natural;
         Right : in Wide_String)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Pos    : Positive         := 1;
         Rlen   : constant Natural := Right'Length;
         Nlen   : constant Natural := Left * Rlen;

      begin
         if Nlen > Max_Length then
            raise Ada.Strings.Index_Error;
         else
            Result.Length := Nlen;

            if Nlen > 0 then
               for J in 1 .. Left loop
                  Result.Data (Pos .. Pos + Rlen - 1) := Right;
                  Pos := Pos + Rlen;
               end loop;
            end if;
         end if;

         return Result;
      end "*";

      function "*"
        (Left  : in Natural;
         Right : in Bounded_Wide_String)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Pos    : Positive := 1;
         Rlen   : constant Length_Range := Right.Length;
         Nlen   : constant Natural      := Left * Rlen;

      begin
         if Nlen > Max_Length then
            raise Ada.Strings.Length_Error;

         else
            Result.Length := Nlen;

            if Nlen > 0 then
               for J in 1 .. Left loop
                  Result.Data (Pos .. Pos + Rlen - 1) :=
                    Right.Data (1 .. Rlen);
                  Pos := Pos + Rlen;
               end loop;
            end if;
         end if;

         return Result;
      end "*";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left  : in Bounded_Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) < Right.Data (1 .. Right.Length);
      end "<";

      function "<"
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) < Right;
      end "<";

      function "<"
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left < Right.Data (1 .. Right.Length);
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left  : in Bounded_Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) <= Right.Data (1 .. Right.Length);
      end "<=";

      function "<="
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) <= Right;
      end "<=";

      function "<="
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left <= Right.Data (1 .. Right.Length);
      end "<=";

      ---------
      -- "=" --
      ---------

      function "="
        (Left  : in Bounded_Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left.Length = Right.Length
           and then Left.Data (1 .. Left.Length) =
                    Right.Data (1 .. Right.Length);
      end "=";

      function "="
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String)
         return  Boolean
      is
      begin
         return Left.Length = Right'Length
           and then Left.Data (1 .. Left.Length) = Right;
      end "=";

      function "="
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left'Length = Right.Length
           and then Left = Right.Data (1 .. Right.Length);
      end "=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left  : in Bounded_Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) > Right.Data (1 .. Right.Length);
      end ">";

      function ">"
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) > Right;
      end ">";

      function ">"
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left > Right.Data (1 .. Right.Length);
      end ">";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left  : in Bounded_Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) >= Right.Data (1 .. Right.Length);
      end ">=";

      function ">="
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String)
         return  Boolean
      is
      begin
         return Left.Data (1 .. Left.Length) >= Right;
      end ">=";

      function ">="
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String)
         return  Boolean
      is
      begin
         return Left >= Right.Data (1 .. Right.Length);
      end ">=";

      ------------
      -- Append --
      ------------

      --  Case of Bounded_Wide_String and Bounded_Wide_String

      function Append
        (Left, Right : in Bounded_Wide_String;
         Drop        : in Strings.Truncation  := Strings.Error)
         return        Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left.Length;
         Rlen   : constant Length_Range := Right.Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen <= Max_Length then
            Result.Length := Nlen;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Llen >= Max_Length then -- only case is Llen = Max_Length
                     Result.Data := Right.Data;

                  else
                     Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
                     Result.Data (Llen + 1 .. Max_Length) :=
                       Right.Data (1 .. Max_Length - Llen);
                  end if;

               when Strings.Left =>
                  if Rlen >= Max_Length then -- only case is Rlen = Max_Length
                     Result.Data := Right.Data;

                  else
                     Result.Data (1 .. Max_Length - Rlen) :=
                       Left.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                     Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                       Right.Data (1 .. Rlen);
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Append;

      procedure Append
        (Source   : in out Bounded_Wide_String;
         New_Item : in Bounded_Wide_String;
         Drop     : in Truncation  := Error)
      is
         Llen   : constant Length_Range := Source.Length;
         Rlen   : constant Length_Range := New_Item.Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen <= Max_Length then
            Source.Length := Nlen;
            Source.Data (Llen + 1 .. Nlen) := New_Item.Data (1 .. Rlen);

         else
            Source.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Llen < Max_Length then
                     Source.Data (Llen + 1 .. Max_Length) :=
                       New_Item.Data (1 .. Max_Length - Llen);
                  end if;

               when Strings.Left =>
                  if Rlen >= Max_Length then -- only case is Rlen = Max_Length
                     Source.Data := New_Item.Data;

                  else
                     Source.Data (1 .. Max_Length - Rlen) :=
                       Source.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                     Source.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                       New_Item.Data (1 .. Rlen);
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

      end Append;

      --  Case of Bounded_Wide_String and Wide_String

      function Append
        (Left  : in Bounded_Wide_String;
         Right : in Wide_String;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left.Length;
         Rlen   : constant Length_Range := Right'Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen <= Max_Length then
            Result.Length := Nlen;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1 .. Nlen) := Right;

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Llen >= Max_Length then -- only case is Llen = Max_Length
                     Result.Data := Left.Data;

                  else
                     Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
                     Result.Data (Llen + 1 .. Max_Length) :=
                       Right (Right'First .. Right'First - 1 +
                                              Max_Length - Llen);

                  end if;

               when Strings.Left =>
                  if Rlen >= Max_Length then
                     Result.Data (1 .. Max_Length) :=
                       Right (Right'Last - (Max_Length - 1) .. Right'Last);

                  else
                     Result.Data (1 .. Max_Length - Rlen) :=
                       Left.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                     Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                       Right;
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Append;

      procedure Append
        (Source   : in out Bounded_Wide_String;
         New_Item : in Wide_String;
         Drop     : in Truncation  := Error)
      is
         Llen   : constant Length_Range := Source.Length;
         Rlen   : constant Length_Range := New_Item'Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen <= Max_Length then
            Source.Length := Nlen;
            Source.Data (Llen + 1 .. Nlen) := New_Item;

         else
            Source.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Llen < Max_Length then
                     Source.Data (Llen + 1 .. Max_Length) :=
                       New_Item (New_Item'First ..
                                       New_Item'First - 1 + Max_Length - Llen);
                  end if;

               when Strings.Left =>
                  if Rlen >= Max_Length then
                     Source.Data (1 .. Max_Length) :=
                       New_Item (New_Item'Last - (Max_Length - 1) ..
                                                                New_Item'Last);

                  else
                     Source.Data (1 .. Max_Length - Rlen) :=
                       Source.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                     Source.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                       New_Item;
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

      end Append;

      --  Case of Wide_String and Bounded_Wide_String

      function Append
        (Left  : in Wide_String;
         Right : in Bounded_Wide_String;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left'Length;
         Rlen   : constant Length_Range := Right.Length;
         Nlen   : constant Natural      := Llen + Rlen;

      begin
         if Nlen <= Max_Length then
            Result.Length := Nlen;
            Result.Data (1 .. Llen) := Left;
            Result.Data (Llen + 1 .. Llen + Rlen) := Right.Data (1 .. Rlen);

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Llen >= Max_Length then
                     Result.Data (1 .. Max_Length) :=
                        Left (Left'First .. Left'First + (Max_Length - 1));

                  else
                     Result.Data (1 .. Llen) := Left;
                     Result.Data (Llen + 1 .. Max_Length) :=
                       Right.Data (1 .. Max_Length - Llen);
                  end if;

               when Strings.Left =>
                  if Rlen >= Max_Length then
                     Result.Data (1 .. Max_Length) :=
                       Right.Data (Rlen - (Max_Length - 1) .. Rlen);

                  else
                     Result.Data (1 .. Max_Length - Rlen) :=
                       Left (Left'Last - (Max_Length - Rlen - 1) .. Left'Last);
                     Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                       Right.Data (1 .. Rlen);
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Append;

      --  Case of Bounded_Wide_String and Wide_Character

      function Append
        (Left  : in Bounded_Wide_String;
         Right : in Wide_Character;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Llen   : constant Length_Range := Left.Length;

      begin
         if Llen  < Max_Length then
            Result.Length := Llen + 1;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1) := Right;
            return Result;

         else
            case Drop is
               when Strings.Right =>
                  return Left;

               when Strings.Left =>
                  Result.Length := Max_Length;
                  Result.Data (1 .. Max_Length - 1) :=
                    Left.Data (2 .. Max_Length);
                  Result.Data (Max_Length) := Right;
                  return Result;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;
      end Append;

      procedure Append
        (Source   : in out Bounded_Wide_String;
         New_Item : in Wide_Character;
         Drop     : in Truncation  := Error)
      is
         Llen   : constant Length_Range := Source.Length;

      begin
         if Llen  < Max_Length then
            Source.Length := Llen + 1;
            Source.Data (Llen + 1) := New_Item;

         else
            Source.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  null;

               when Strings.Left =>
                  Source.Data (1 .. Max_Length - 1) :=
                    Source.Data (2 .. Max_Length);
                  Source.Data (Max_Length) := New_Item;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

      end Append;

      --  Case of Wide_Character and Bounded_Wide_String

      function Append
        (Left  : in Wide_Character;
         Right : in Bounded_Wide_String;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Rlen   : constant Length_Range := Right.Length;

      begin
         if Rlen < Max_Length then
            Result.Length := Rlen + 1;
            Result.Data (1) := Left;
            Result.Data (2 .. Rlen + 1) := Right.Data (1 .. Rlen);
            return Result;

         else
            case Drop is
               when Strings.Right =>
                  Result.Length := Max_Length;
                  Result.Data (1) := Left;
                  Result.Data (2 .. Max_Length) :=
                    Right.Data (1 .. Max_Length - 1);
                  return Result;

               when Strings.Left =>
                  return Right;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;
      end Append;

      -----------
      -- Count --
      -----------

      function Count
        (Source  : in Bounded_Wide_String;
         Pattern : in Wide_String;
         Mapping : in Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
         return    Natural
      is
      begin
         return
           Wide_Search.Count
             (Source.Data (1 .. Source.Length), Pattern, Mapping);
      end Count;

      function Count
        (Source  : in Bounded_Wide_String;
         Pattern : in Wide_String;
         Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
         return    Natural
      is
      begin
         return
           Wide_Search.Count
             (Source.Data (1 .. Source.Length), Pattern, Mapping);
      end Count;

      function Count
        (Source : in Bounded_Wide_String;
         Set    : in Wide_Maps.Wide_Character_Set)
         return   Natural
      is
      begin
         return Wide_Search.Count (Source.Data (1 .. Source.Length), Set);
      end Count;

      ------------
      -- Delete --
      ------------

      function Delete
        (Source  : in Bounded_Wide_String;
         From    : in Positive;
         Through : in Natural)
         return    Bounded_Wide_String
      is
         Slen       : constant Natural := Source.Length;
         Num_Delete : constant Integer := Through - From + 1;
         Result     : Bounded_Wide_String;

      begin
         if Num_Delete <= 0 then
            return Source;

         elsif From > Slen + 1 then
            raise Ada.Strings.Index_Error;

         elsif Through >= Slen then
            Result.Length := From - 1;
            Result.Data (1 .. From - 1) := Source.Data (1 .. From - 1);
            return Result;

         else
            Result.Length := Slen - Num_Delete;
            Result.Data (1 .. From - 1) := Source.Data (1 .. From - 1);
            Result.Data (From .. Result.Length) :=
              Source.Data (Through + 1 .. Slen);
            return Result;
         end if;
      end Delete;

      procedure Delete
        (Source  : in out Bounded_Wide_String;
         From    : in Positive;
         Through : in Natural)
      is
         Slen       : constant Natural := Source.Length;
         Num_Delete : constant Integer := Through - From + 1;

      begin
         if Num_Delete <= 0 then
            return;

         elsif From > Slen + 1 then
            raise Ada.Strings.Index_Error;

         elsif Through >= Slen then
            Source.Length := From - 1;

         else
            Source.Length := Slen - Num_Delete;
            Source.Data (From .. Source.Length) :=
              Source.Data (Through + 1 .. Slen);
         end if;
      end Delete;

      -------------
      -- Element --
      -------------

      function Element
        (Source : in Bounded_Wide_String;
         Index  : in Positive)
         return   Wide_Character
      is
      begin
         if Index in 1 .. Source.Length then
            return Source.Data (Index);
         else
            raise Strings.Index_Error;
         end if;
      end Element;

      ----------------
      -- Find_Token --
      ----------------

      procedure Find_Token
        (Source : in Bounded_Wide_String;
         Set    : in Wide_Maps.Wide_Character_Set;
         Test   : in Strings.Membership;
         First  : out Positive;
         Last   : out Natural)
      is
      begin
         Wide_Search.Find_Token
           (Source.Data (1 .. Source.Length), Set, Test, First, Last);
      end Find_Token;


      ----------
      -- Head --
      ----------

      function Head
        (Source : in Bounded_Wide_String;
         Count  : in Natural;
         Pad    : in Wide_Character := Wide_Space;
         Drop   : in Strings.Truncation := Strings.Error)
         return   Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Slen   : constant Natural := Source.Length;
         Npad   : constant Integer := Count - Slen;

      begin
         if Npad <= 0 then
            Result.Length := Count;
            Result.Data (1 .. Count) := Source.Data (1 .. Count);

         elsif Count <= Max_Length then
            Result.Length := Count;
            Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
            Result.Data (Slen + 1 .. Count) := (others => Pad);

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
                  Result.Data (Slen + 1 .. Max_Length) := (others => Pad);

               when Strings.Left =>
                  if Npad >= Max_Length then
                     Result.Data := (others => Pad);

                  else
                     Result.Data (1 .. Max_Length - Npad) :=
                       Source.Data (Count - Max_Length + 1 .. Slen);
                     Result.Data (Max_Length - Npad + 1 .. Max_Length) :=
                       (others => Pad);
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Head;

      procedure Head
        (Source : in out Bounded_Wide_String;
         Count  : in Natural;
         Pad    : in Wide_Character  := Wide_Space;
         Drop   : in Truncation := Error)
      is
         Slen   : constant Natural := Source.Length;
         Npad   : constant Integer := Count - Slen;
         Temp   : Wide_String (1 .. Max_Length);

      begin
         if Npad <= 0 then
            Source.Length := Count;

         elsif Count <= Max_Length then
            Source.Length := Count;
            Source.Data (Slen + 1 .. Count) := (others => Pad);

         else
            Source.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  Source.Data (Slen + 1 .. Max_Length) := (others => Pad);

               when Strings.Left =>
                  if Npad > Max_Length then
                     Source.Data := (others => Pad);

                  else
                     Temp := Source.Data;
                     Source.Data (1 .. Max_Length - Npad) :=
                       Temp (Count - Max_Length + 1 .. Slen);

                     for J in Max_Length - Npad + 1 .. Max_Length loop
                        Source.Data (J) := Pad;
                     end loop;
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

      end Head;

      -----------
      -- Index --
      -----------

      function Index
        (Source  : in Bounded_Wide_String;
         Pattern : in Wide_String;
         Going   : in Strings.Direction := Strings.Forward;
         Mapping : in Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
         return    Natural
      is
      begin
         return Wide_Search.Index
           (Source.Data (1 .. Source.Length), Pattern, Going, Mapping);
      end Index;

      function Index
        (Source  : in Bounded_Wide_String;
         Pattern : in Wide_String;
         Going   : in Direction := Forward;
         Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
         return    Natural
      is
      begin
         return Wide_Search.Index
           (Source.Data (1 .. Source.Length), Pattern, Going, Mapping);
      end Index;

      function Index
        (Source : in Bounded_Wide_String;
         Set    : in Wide_Maps.Wide_Character_Set;
         Test   : in Strings.Membership := Strings.Inside;
         Going  : in Strings.Direction  := Strings.Forward)
         return   Natural
      is
      begin
         return Wide_Search.Index
           (Source.Data (1 .. Source.Length), Set, Test, Going);
      end Index;

      ---------------------
      -- Index_Non_Blank --
      ---------------------

      function Index_Non_Blank
        (Source : in Bounded_Wide_String;
         Going  : in Strings.Direction := Strings.Forward)
         return   Natural
      is
      begin
         return
           Wide_Search.Index_Non_Blank
             (Source.Data (1 .. Source.Length), Going);
      end Index_Non_Blank;

      ------------
      -- Insert --
      ------------

      function Insert
        (Source   : in Bounded_Wide_String;
         Before   : in Positive;
         New_Item : in Wide_String;
         Drop     : in Strings.Truncation := Strings.Error)
         return     Bounded_Wide_String
      is
         Slen    : constant Natural := Source.Length;
         Nlen    : constant Natural := New_Item'Length;
         Tlen    : constant Natural := Slen + Nlen;
         Blen    : constant Natural := Before - 1;
         Alen    : constant Integer := Slen - Blen;
         Droplen : constant Integer := Tlen - Max_Length;
         Result  : Bounded_Wide_String;

         --  Tlen is the length of the total string before possible truncation.
         --  Blen, Alen are the lengths of the before and after pieces of the
         --  source string.

      begin
         if Alen < 0 then
            raise Ada.Strings.Index_Error;

         elsif Droplen <= 0 then
            Result.Length := Tlen;
            Result.Data (1 .. Blen) := Source.Data (1 .. Blen);
            Result.Data (Before .. Before + Nlen - 1) := New_Item;
            Result.Data (Before + Nlen .. Tlen) :=
              Source.Data (Before .. Slen);

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  Result.Data (1 .. Blen) := Source.Data (1 .. Blen);

                  if Droplen > Alen then
                     Result.Data (Before .. Max_Length) :=
                       New_Item (New_Item'First
                                   .. New_Item'First + Max_Length - Before);
                  else
                     Result.Data (Before .. Before + Nlen - 1) := New_Item;
                     Result.Data (Before + Nlen .. Max_Length) :=
                       Source.Data (Before .. Slen - Droplen);
                  end if;

               when Strings.Left =>
                  Result.Data (Max_Length - (Alen - 1) .. Max_Length) :=
                    Source.Data (Before .. Slen);

                  if Droplen >= Blen then
                     Result.Data (1 .. Max_Length - Alen) :=
                       New_Item (New_Item'Last - (Max_Length - Alen) + 1
                                   .. New_Item'Last);
                  else
                     Result.Data
                       (Blen - Droplen + 1 .. Max_Length - Alen) :=
                         New_Item;
                     Result.Data (1 .. Blen - Droplen) :=
                       Source.Data (Droplen + 1 .. Blen);
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Insert;

      procedure Insert
        (Source   : in out Bounded_Wide_String;
         Before   : in Positive;
         New_Item : in Wide_String;
         Drop     : in Strings.Truncation := Strings.Error)
      is
      begin
         --  We do a double copy here because this is one of the situations
         --  in which we move data to the right, and at least at the moment,
         --  GNAT is not handling such cases correctly ???

         Source := Insert (Source, Before, New_Item, Drop);
      end Insert;

      ------------
      -- Length --
      ------------

      function Length (Source : in Bounded_Wide_String) return Length_Range is
      begin
         return Source.Length;
      end Length;

      ---------------
      -- Overwrite --
      ---------------

      function Overwrite
        (Source    : in Bounded_Wide_String;
         Position  : in Positive;
         New_Item  : in Wide_String;
         Drop      : in Strings.Truncation := Strings.Error)
         return      Bounded_Wide_String
      is
         Result  : Bounded_Wide_String;
         Endpos  : constant Natural  := Position + New_Item'Length - 1;
         Slen    : constant Natural  := Source.Length;
         Droplen : Natural;

      begin
         if Position > Slen + 1 then
            raise Ada.Strings.Index_Error;

         elsif New_Item'Length = 0 then
            return Source;

         elsif Endpos <= Slen then
            Result.Length := Source.Length;
            Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
            Result.Data (Position .. Endpos) := New_Item;
            return Result;

         elsif Endpos <= Max_Length then
            Result.Length := Endpos;
            Result.Data (1 .. Position - 1) := Source.Data (1 .. Position - 1);
            Result.Data (Position .. Endpos) := New_Item;
            return Result;

         else
            Result.Length := Max_Length;
            Droplen := Endpos - Max_Length;

            case Drop is
               when Strings.Right =>
                  Result.Data (1 .. Position - 1) :=
                    Source.Data (1 .. Position - 1);

                  Result.Data (Position .. Max_Length) :=
                    New_Item (New_Item'First .. New_Item'Last - Droplen);
                  return Result;

               when Strings.Left =>
                  if New_Item'Length >= Max_Length then
                     Result.Data (1 .. Max_Length) :=
                        New_Item (New_Item'Last - Max_Length + 1 ..
                                  New_Item'Last);
                     return Result;

                  else
                     Result.Data (1 .. Max_Length - New_Item'Length) :=
                       Source.Data (Droplen + 1 .. Position - 1);
                     Result.Data
                       (Max_Length - New_Item'Length + 1 .. Max_Length) :=
                         New_Item;
                     return Result;
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;
      end Overwrite;

      procedure Overwrite
        (Source    : in out Bounded_Wide_String;
         Position  : in Positive;
         New_Item  : in Wide_String;
         Drop      : in Strings.Truncation := Strings.Error)
      is
         Endpos  : constant Positive := Position + New_Item'Length - 1;
         Slen    : constant Natural  := Source.Length;
         Droplen : Natural;

      begin
         if Position > Slen + 1 then
            raise Ada.Strings.Index_Error;

         elsif Endpos <= Slen then
            Source.Data (Position .. Endpos) := New_Item;

         elsif Endpos <= Max_Length then
            Source.Data (Position .. Endpos) := New_Item;
            Source.Length := Endpos;

         else
            Source.Length := Max_Length;
            Droplen := Endpos - Max_Length;

            case Drop is
               when Strings.Right =>
                  Source.Data (Position .. Max_Length) :=
                    New_Item (New_Item'First .. New_Item'Last - Droplen);

               when Strings.Left =>
                  if New_Item'Length > Max_Length then
                     Source.Data (1 .. Max_Length) :=
                        New_Item (New_Item'Last - Max_Length + 1 ..
                                  New_Item'Last);

                  else
                     Source.Data (1 .. Max_Length - New_Item'Length) :=
                       Source.Data (Droplen + 1 .. Position - 1);

                     Source.Data
                       (Max_Length - New_Item'Length + 1 .. Max_Length) :=
                         New_Item;
                  end if;

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;
      end Overwrite;

      ---------------------
      -- Replace_Element --
      ---------------------

      procedure Replace_Element
        (Source : in out Bounded_Wide_String;
         Index  : in Positive;
         By     : in Wide_Character)
      is
      begin
         if Index <= Source.Length then
            Source.Data (Index) := By;
         else
            raise Ada.Strings.Index_Error;
         end if;
      end Replace_Element;

      -------------------
      -- Replace_Slice --
      -------------------

      function Replace_Slice
        (Source   : in Bounded_Wide_String;
         Low      : in Positive;
         High     : in Natural;
         By       : in Wide_String;
         Drop     : in Strings.Truncation := Strings.Error)
         return     Bounded_Wide_String
      is
         Slen : constant Natural := Source.Length;

      begin
         if Low > Slen + 1 then
            raise Strings.Index_Error;

         elsif High < Low then
            return Insert (Source, Low, By, Drop);

         else
            declare
               Blen    : constant Natural := Natural'Max (0, Low - 1);
               Alen    : constant Natural := Natural'Max (0, Slen - High);
               Tlen    : constant Natural := Blen + By'Length + Alen;
               Droplen : constant Integer := Tlen - Max_Length;
               Result  : Bounded_Wide_String;

               --  Tlen is the total length of the result string before any
               --  truncation. Blen and Alen are the lengths of the pieces
               --  of the original string that end up in the result string
               --  before and after the replaced slice.

            begin
               if Droplen <= 0 then
                  Result.Length := Tlen;
                  Result.Data (1 .. Blen) := Source.Data (1 .. Blen);
                  Result.Data (Low .. Low + By'Length - 1) := By;
                  Result.Data (Low + By'Length .. Tlen) :=
                    Source.Data (High + 1 .. Slen);

               else
                  Result.Length := Max_Length;

                  case Drop is
                     when Strings.Right =>
                        Result.Data (1 .. Blen) := Source.Data (1 .. Blen);

                        if Droplen > Alen then
                           Result.Data (Low .. Max_Length) :=
                             By (By'First .. By'First + Max_Length - Low);
                        else
                           Result.Data (Low .. Low + By'Length - 1) := By;
                           Result.Data (Low + By'Length .. Max_Length) :=
                             Source.Data (High + 1 .. Slen - Droplen);
                        end if;

                     when Strings.Left =>
                        Result.Data (Max_Length - (Alen - 1) .. Max_Length) :=
                          Source.Data (High + 1 .. Slen);

                        if Droplen >= Blen then
                           Result.Data (1 .. Max_Length - Alen) :=
                             By (By'Last - (Max_Length - Alen) + 1 .. By'Last);
                        else
                           Result.Data
                             (Blen - Droplen + 1 .. Max_Length - Alen) := By;
                           Result.Data (1 .. Blen - Droplen) :=
                             Source.Data (Droplen + 1 .. Blen);
                        end if;

                     when Strings.Error =>
                        raise Ada.Strings.Length_Error;
                  end case;
               end if;

               return Result;
            end;
         end if;
      end Replace_Slice;

      procedure Replace_Slice
        (Source   : in out Bounded_Wide_String;
         Low      : in Positive;
         High     : in Natural;
         By       : in Wide_String;
         Drop     : in Strings.Truncation := Strings.Error)
      is
      begin
         --  We do a double copy here because this is one of the situations
         --  in which we move data to the right, and at least at the moment,
         --  GNAT is not handling such cases correctly ???

         Source := Replace_Slice (Source, Low, High, By, Drop);
      end Replace_Slice;

      ---------------
      -- Replicate --
      ---------------

      function Replicate
        (Count : in Natural;
         Item  : in Wide_Character;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
         Result : Bounded_Wide_String;

      begin
         if Count <= Max_Length then
            Result.Length := Count;

         elsif Drop = Strings.Error then
            raise Ada.Strings.Length_Error;

         else
            Result.Length := Max_Length;
         end if;

         Result.Data (1 .. Result.Length) := (others => Item);
         return Result;
      end Replicate;

      function Replicate
        (Count : in Natural;
         Item  : in Wide_String;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
         Length : constant Integer := Count * Item'Length;
         Result : Bounded_Wide_String;
         Indx   : Positive;

      begin
         if Length <= Max_Length then
            Result.Length := Length;

            if Length > 0 then
               Indx := 1;

               for J in 1 .. Count loop
                  Result.Data (Indx .. Indx + Item'Length - 1) := Item;
                  Indx := Indx + Item'Length;
               end loop;
            end if;

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  Indx := 1;

                  while Indx + Item'Length <= Max_Length + 1 loop
                     Result.Data (Indx .. Indx + Item'Length - 1) := Item;
                     Indx := Indx + Item'Length;
                  end loop;

                  Result.Data (Indx .. Max_Length) :=
                    Item (Item'First .. Item'First + Max_Length - Indx);

               when Strings.Left =>
                  Indx := Max_Length;

                  while Indx - Item'Length >= 1 loop
                     Result.Data (Indx - (Item'Length - 1) .. Indx) := Item;
                     Indx := Indx - Item'Length;
                  end loop;

                  Result.Data (1 .. Indx) :=
                    Item (Item'Last - Indx + 1 .. Item'Last);

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Replicate;

      function Replicate
        (Count : in Natural;
         Item  : in Bounded_Wide_String;
         Drop  : in Strings.Truncation := Strings.Error)
         return  Bounded_Wide_String
      is
      begin
         return Replicate (Count, Item.Data (1 .. Item.Length), Drop);
      end Replicate;

      -----------
      -- Slice --
      -----------

      function Slice
        (Source : Bounded_Wide_String;
         Low    : Positive;
         High   : Natural)
         return   Wide_String
      is
      begin
         --  Note: test of High > Length is in accordance with AI95-00128

         if Low > Source.Length + 1 or else High > Source.Length then
            raise Index_Error;

         else
            declare
               Result : Wide_String (1 .. High - Low + 1);

            begin
               Result := Source.Data (Low .. High);
               return Result;
            end;
         end if;
      end Slice;

      ----------
      -- Tail --
      ----------

      function Tail
        (Source : in Bounded_Wide_String;
         Count  : in Natural;
         Pad    : in Wide_Character := Wide_Space;
         Drop   : in Strings.Truncation := Strings.Error)
         return   Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Slen   : constant Natural := Source.Length;
         Npad   : constant Integer := Count - Slen;

      begin
         if Npad <= 0 then
            Result.Length := Count;
            Result.Data (1 .. Count) :=
              Source.Data (Slen - (Count - 1) .. Slen);

         elsif Count <= Max_Length then
            Result.Length := Count;
            Result.Data (1 .. Npad) := (others => Pad);
            Result.Data (Npad + 1 .. Count) := Source.Data (1 .. Slen);

         else
            Result.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Npad >= Max_Length then
                     Result.Data := (others => Pad);

                  else
                     Result.Data (1 .. Npad) := (others => Pad);
                     Result.Data (Npad + 1 .. Max_Length) :=
                       Source.Data (1 .. Max_Length - Npad);
                  end if;

               when Strings.Left =>
                  Result.Data (1 .. Max_Length - Slen) := (others => Pad);
                  Result.Data (Max_Length - Slen + 1 .. Max_Length) :=
                    Source.Data (1 .. Slen);

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end Tail;

      procedure Tail
        (Source : in out Bounded_Wide_String;
         Count  : in Natural;
         Pad    : in Wide_Character  := Wide_Space;
         Drop   : in Truncation := Error)
      is
         Slen   : constant Natural := Source.Length;
         Npad   : constant Integer := Count - Slen;
         Temp   : Wide_String (1 .. Max_Length) := Source.Data;

      begin
         if Npad <= 0 then
            Source.Length := Count;
            Source.Data (1 .. Count) :=
              Temp (Slen - (Count - 1) .. Slen);

         elsif Count <= Max_Length then
            Source.Length := Count;
            Source.Data (1 .. Npad) := (others => Pad);
            Source.Data (Npad + 1 .. Count) := Temp (1 .. Slen);

         else
            Source.Length := Max_Length;

            case Drop is
               when Strings.Right =>
                  if Npad >= Max_Length then
                     Source.Data := (others => Pad);

                  else
                     Source.Data (1 .. Npad) := (others => Pad);
                     Source.Data (Npad + 1 .. Max_Length) :=
                       Temp (1 .. Max_Length - Npad);
                  end if;

               when Strings.Left =>
                  for J in 1 .. Max_Length - Slen loop
                     Source.Data (J) := Pad;
                  end loop;

                  Source.Data (Max_Length - Slen + 1 .. Max_Length) :=
                    Temp (1 .. Slen);

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

      end Tail;

      ----------------------------
      -- To_Bounded_Wide_String --
      ----------------------------

      function To_Bounded_Wide_String
        (Source : in Wide_String;
         Drop   : in Strings.Truncation := Strings.Error)
         return   Bounded_Wide_String
      is
         Slen   : constant Natural := Source'Length;
         Result : Bounded_Wide_String;

      begin
         if Slen <= Max_Length then
            Result.Length := Slen;
            Result.Data (1 .. Slen) := Source;

         else
            case Drop is
               when Strings.Right =>
                  Result.Length := Max_Length;
                  Result.Data (1 .. Max_Length) :=
                    Source (Source'First .. Source'First - 1 + Max_Length);

               when Strings.Left =>
                  Result.Length := Max_Length;
                  Result.Data (1 .. Max_Length) :=
                    Source (Source'Last - (Max_Length - 1) .. Source'Last);

               when Strings.Error =>
                  raise Ada.Strings.Length_Error;
            end case;
         end if;

         return Result;
      end To_Bounded_Wide_String;

      --------------------
      -- To_Wide_String --
      --------------------

      function To_Wide_String
        (Source : in Bounded_Wide_String)
         return   Wide_String
      is
      begin
         return Source.Data (1 .. Source.Length);
      end To_Wide_String;

      ---------------
      -- Translate --
      ---------------

      function Translate
        (Source  : in Bounded_Wide_String;
         Mapping : in Wide_Maps.Wide_Character_Mapping)
         return    Bounded_Wide_String
      is
         Result : Bounded_Wide_String;

      begin
         Result.Length := Source.Length;

         for J in 1 .. Source.Length loop
            Result.Data (J) := Value (Mapping, Source.Data (J));
         end loop;

         return Result;
      end Translate;

      procedure Translate
        (Source   : in out Bounded_Wide_String;
         Mapping  : in Wide_Maps.Wide_Character_Mapping)
      is
      begin
         for J in 1 .. Source.Length loop
            Source.Data (J) := Value (Mapping, Source.Data (J));
         end loop;
      end Translate;

      function Translate
        (Source  : in Bounded_Wide_String;
         Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
         return    Bounded_Wide_String
      is
         Result : Bounded_Wide_String;

      begin
         Result.Length := Source.Length;

         for J in 1 .. Source.Length loop
            Result.Data (J) := Mapping.all (Source.Data (J));
         end loop;

         return Result;
      end Translate;

      procedure Translate
        (Source  : in out Bounded_Wide_String;
         Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
      is
      begin
         for J in 1 .. Source.Length loop
            Source.Data (J) := Mapping.all (Source.Data (J));
         end loop;
      end Translate;

      ----------
      -- Trim --
      ----------

      function Trim
        (Source : in Bounded_Wide_String;
         Side   : in Trim_End)
         return   Bounded_Wide_String
      is
         Result : Bounded_Wide_String;
         Last   : Natural := Source.Length;
         First  : Positive := 1;

      begin
         if Side = Left or else Side = Both then
            while First <= Last and then Source.Data (First) = ' ' loop
               First := First + 1;
            end loop;
         end if;

         if Side = Right or else Side = Both then
            while Last >= First and then Source.Data (Last) = ' ' loop
               Last := Last - 1;
            end loop;
         end if;

         Result.Length := Last - First + 1;
         Result.Data (1 .. Result.Length) := Source.Data (First .. Last);
         return Result;

      end Trim;

      procedure Trim
        (Source : in out Bounded_Wide_String;
         Side   : in Trim_End)
      is
         Last   : Length_Range := Source.Length;
         First  : Positive     := 1;
         Temp   : Wide_String (1 .. Max_Length);

      begin
         Temp (1 .. Last) := Source.Data (1 .. Last);

         if Side = Left or else Side = Both then
            while First <= Last and then Temp (First) = ' ' loop
               First := First + 1;
            end loop;
         end if;

         if Side = Right or else Side = Both then
            while Last >= First and then Temp (Last) = ' ' loop
               Last := Last - 1;
            end loop;
         end if;

         Source.Length := Last - First + 1;
         Source.Data (1 .. Source.Length) := Temp (First .. Last);

      end Trim;

      function Trim
        (Source : in Bounded_Wide_String;
         Left   : in Wide_Maps.Wide_Character_Set;
         Right  : in Wide_Maps.Wide_Character_Set)
         return   Bounded_Wide_String
      is
         Result : Bounded_Wide_String;

      begin
         for First in 1 .. Source.Length loop
            if not Is_In (Source.Data (First), Left) then
               for Last in reverse First .. Source.Length loop
                  if not Is_In (Source.Data (Last), Right) then
                     Result.Length := Last - First + 1;
                     Result.Data (1 .. Result.Length) :=
                        Source.Data (First .. Last);
                     return Result;
                  end if;
               end loop;
            end if;
         end loop;

         Result.Length := 0;
         return Result;
      end Trim;

      procedure Trim
        (Source : in out Bounded_Wide_String;
         Left   : in Wide_Maps.Wide_Character_Set;
         Right  : in Wide_Maps.Wide_Character_Set)
      is
      begin
         for First in 1 .. Source.Length loop
            if not Is_In (Source.Data (First), Left) then
               for Last in reverse First .. Source.Length loop
                  if not Is_In (Source.Data (Last), Right) then
                     if First = 1 then
                        Source.Length := Last;
                        return;
                     else
                        Source.Length := Last - First + 1;
                        Source.Data (1 .. Source.Length) :=
                          Source.Data (First .. Last);
                        return;
                     end if;
                  end if;
               end loop;

               Source.Length := 0;
               return;
            end if;
         end loop;

         Source.Length := 0;
      end Trim;

   end Generic_Bounded_Length;

end Ada.Strings.Wide_Bounded;

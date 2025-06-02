-- FC54A00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation declares various types which will serve as designated
--      types for tests involving generic formal access types (including
--      access-to-subprogram types).
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package FC54A00 is


   -- Discrete (integer) types:

   Bits : constant := 8;                    -- Named number.

   type    Numerals           is range -256 .. 255;
   type    New_Numerals       is new Numerals range -128 .. 127;
   subtype Positives          is Numerals range 0 .. 255;
   subtype Same_Numerals      is Numerals;
   subtype Numerals_Static    is Numerals range -2**Bits .. 2**Bits - 1;

   Min  : Numerals  := Numerals'First;      -- Variable.
   Max  : Integer   := 255;                 -- Variable.

   subtype Numerals_Nonstatic is Numerals  range Min .. 255;
   subtype Positive_Nonstatic is Positives range 0 .. Positives(Max);
   subtype Pos_Dupl_Nonstatic is Positives range 0 .. Positives(Max);
   subtype Pos_Attr_Nonstatic is Positives range Positive_Nonstatic'Range;



   -- Floating point types:

   type    Float_Type       is digits 3;
   type    New_Float        is new Float_Type;
   subtype Float_100        is Float_Type range 0.0 .. 100.0;
   subtype Same_Float       is Float_Type;

   Hundred : constant := 100.0;             -- Named number.

   type    Float_With_Range is digits 3 range 0.0 .. 100.0;
   subtype Float_Same_Range is Float_With_Range range 0.0 .. Hundred;



   -- Tagged record types:

   subtype Lengths is Natural range 0 .. 50;

   type Parent is abstract tagged null record;

   type Tag (Len: Lengths) is new Parent with record
      Msg : String (1 .. Len);
   end record;

   type New_Tag is new Tag with record
      Sent : Boolean;
   end record;

   subtype Same_Tag is Tag;

   Twenty : constant := 20;                 -- Named number.

   subtype Tag20      is Tag (Len => 20);
   subtype Tag25      is Tag (25);
   subtype Tag_Twenty is Tag (Twenty);

   My_Len : Lengths := Twenty;              -- Variable.
   subtype Sub_Length is Lengths range 1 .. My_Len;

   subtype Tag20_Nonstatic      is Tag (Len => Sub_Length'Last);
   subtype Tag20_Dupl_Nonstatic is Tag (Sub_Length'Last);
   subtype Tag20_Same_Nonstatic is Tag20_Nonstatic;
   subtype Tag20_Var_Nonstatic  is Tag (Len => My_Len);



   -- Access types (designated type is tagged):

   type Tagged_Ptr         is access Tag;
   type Tag_Class_Ptr      is access Tag'Class;

   subtype Msg_Ptr_Static  is Tagged_Ptr(Twenty);



   -- Array types:

   type    New_String  is new String;
   subtype Same_String is String;

   Ten : constant := 10;                    -- Named number.

   subtype Msg_Static  is     String(1 .. Ten);
   type    Msg10       is new String(1 .. 10);
   subtype Msg20       is     String(1 .. 20);

   Size : Positive := 10;

   subtype Msg_Nonstatic      is String(1 .. Size);
   subtype Msg_Dupl_Nonstatic is String(1 .. Size);
   subtype Msg_Same_Nonstatic is Msg_Nonstatic;


end FC54A00;

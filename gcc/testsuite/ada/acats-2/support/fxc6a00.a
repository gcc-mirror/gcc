-- FXC6A00.A
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
--     This foundation declares various volatile and non-volatile types. Some
--     are by-reference types, and some allow pass-by-copy.
--
-- CHANGE HISTORY:
--      23 Jan 96   SAIC    Initial version for ACVC 2.1.
--      02 DEC 97   EDS     Removed Pragma Volatile applied to composite types.
--      27 AUG 99   RLB     Repaired so Nonvolatile_Tagged really is
--                          Nonvolatile.
--!

package FXC6A00 is

   type Roman is ('I', 'V', 'X', 'L', 'C', 'D', 'M');  -- By-copy type.

   type Acc_Roman is access all Roman;


   type Tagged_Type is tagged record                   -- By-reference type.
      C: Natural;
   end record;


   type Volatile_Tagged is new Tagged_Type with record -- Volatile by-reference
      R1: Roman;                                       -- type.
   end record;
   pragma Volatile (Volatile_Tagged);

   type Acc_Volatile_Tagged is access all Volatile_Tagged;

                                                       -- By-reference type.
   type NonVolatile_Tagged is new Tagged_Type with record
      R2: aliased Roman;
   end record;


   task type Task_Type is                              -- By-reference type.
      entry Calculate (C: in out Natural);
   end Task_Type;

   type Acc_Task_Type is access all Task_Type;


   protected type Protected_Type is                    -- By-reference type.
      procedure Op;
   private
      Count : Natural := 0;
   end Protected_Type;


   protected type Volatile_Protected is                -- Volatile by-reference
      procedure Handler;                               -- type.
      pragma Interrupt_Handler (Handler);

      function Handled return Boolean;
   private
      Was_Handled : Boolean := False;
   end Volatile_Protected;
   pragma Volatile (Volatile_Protected);

   type Acc_Vol_Protected is access all Volatile_Protected;


   type Record_Type is record                          -- Allows pass-by-copy.
      C: String(1 .. 2);
   end record;


   type Volatile_Record is limited record              -- Volatile by-reference
      C: String(1 .. 2);                               -- type.
   end record;
   pragma Volatile (Volatile_Record);


   type Composite_Type is record                       -- By-reference type.
      C: Tagged_Type;
      D: aliased Volatile_Tagged;                      -- Volatile component.
   end record;


   type Private_Type is private;                       -- By-reference type.


   type Array_Type is array (1..3) of Tagged_Type;     -- By-reference type.
   pragma Volatile_Components (Array_Type);

   type Acc_Array_Type is access all Array_Type;


   type Lim_Private_Type is limited private;           -- By-copy type.

private

   type Private_Type is new Tagged_Type with record
      D: Character;
   end record;


   type Lim_Private_Type is new Integer;

end FXC6A00;


     --==================================================================--


package body FXC6A00 is

   task body Task_Type is
   begin
      accept Calculate (C: in out Natural) do
         C := C * 10;
      end Calculate;
   end Task_Type;


   protected body Protected_Type is
      procedure Op is
      begin
         Count := Count + 1;
      end Op;
   end Protected_Type;


   protected body Volatile_Protected is
      procedure Handler is
      begin
         Was_Handled := True;
      end Handler;

      function Handled return Boolean is
      begin
         return Was_Handled;
      end Handled;
   end Volatile_Protected;

end FXC6A00;

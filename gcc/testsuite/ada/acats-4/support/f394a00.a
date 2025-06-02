-- F394A00.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- FOUNDATION DESCRIPTION:
--      This foundation declares various types used in checking the
--      legality of types derived from progenitors of various kinds.
--
-- CHANGE HISTORY:
--     28 Oct 2007 RLB Created foundation to reduce duplicate code.
--
package F394A00 is

   type Non_Lim_Interface is interface;

   type Lim_Interface is limited interface;

   type Sync_Interface is synchronized interface;

   type Task_Interface is task interface;

   type Prot_Interface is protected interface;

   type Abstract_Non_Lim_Record is abstract tagged null record;
      -- Similar to, but not an interface.

   type Abstract_Lim_Record is abstract tagged limited null record;
      -- Similar to, but not an interface.

   type Abstract_Non_Lim_Private is abstract tagged private;

   type Abstract_Lim_Private is abstract tagged limited private;

   type Tagged_Non_Lim_Record is tagged record
      Count : Natural;
   end record;

   type Tagged_Lim_Record is tagged limited record
      Count : Natural;
   end record;

   type Non_Lim_Intf_Der is new Lim_Interface with null record;

   type Lim_Intf_Der is limited new Lim_Interface with null record;

   type An_Int is range 0 .. 1000;

   type A_Fixed is delta 0.01 range 0.0 .. 10.00;

   type A_Float is digits 4;

   type An_Enum is (Red, Blue, Green);

   type An_Array_of_Int is array (1 .. 10) of An_Int;

   type An_Array_of_Tagged is array (1 .. 10) of Tagged_Lim_Record;

   protected type Prot_Typ is new Sync_Interface with
      entry     Set (Value : in  Integer);
      procedure Get (Value : out Integer);
   private
      Data : Integer := 0;
   end Prot_Typ;

   task type Task_Typ is new Sync_Interface with
      entry Set (Value : in  Integer);
      entry Get (Value : out Integer);
   end Task_Typ;

private
    type Abstract_Non_Lim_Private is abstract tagged null record;
    type Abstract_Lim_Private is abstract tagged limited null record;
end F394A00;


-- CC51B03.A
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
--      Check that the attribute S'Definite, where S is an indefinite formal
--      private or derived type, returns true if the actual corresponding to
--      S is definite, and returns false otherwise.
--
-- TEST DESCRIPTION:
--      A definite subtype is any subtype which is not indefinite. An
--      indefinite subtype is either:
--         a) An unconstrained array subtype.
--         b) A subtype with unknown discriminants (this includes class-wide
--            types).
--         c) A subtype with unconstrained discriminants without defaults.
--
--      The possible forms of indefinite formal subtype are as follows:
--
--         Formal derived types:
--          X - Ancestor is an unconstrained array type
--          * - Ancestor is a discriminated record type without defaults
--          X - Ancestor is a discriminated tagged type
--          * - Ancestor type has unknown discriminants
--            - Formal type has an unknown discriminant part
--          * - Formal type has a known discriminant part
--
--         Formal private types:
--            - Formal type has an unknown discriminant part
--          * - Formal type has a known discriminant part
--
--      The formal subtypes preceded by an 'X' above are not covered, because
--      other rules prevent a definite subtype from being passed as an actual.
--      The formal subtypes preceded by an '*' above are not covered, because
--      'Definite is less likely to be used for these formals.
--
--      The following kinds of actuals are passed to various of the formal
--      types listed above:
--
--            - Undiscriminated type
--            - Type with defaulted discriminants
--            - Type with undefaulted discriminants
--            - Class-wide type
--
--      A typical usage of S'Definite might be algorithm selection in a
--      generic I/O package, e.g., the use of fixed-length or variable-length
--      records depending on whether the actual is definite or indefinite.
--      In such situations, S'Definite would appear in if conditions or other
--      contexts requiring a boolean expression. This test checks S'Definite
--      in such usage contexts but, for brevity, omits any surrounding
--      usage code.
--      
-- TEST FILES:
--      The following files comprise this test:
--
--         FC51B00.A
--      -> CC51B03.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with FC51B00;  -- Indefinite subtype declarations.
package CC51B03_0 is

   --
   -- Formal private type cases:
   --

   generic
      type Formal (<>) is private;              -- Formal has unknown
   package PrivateFormalUnknownDiscriminants is -- discriminant part.
      function Is_Definite return Boolean;
   end PrivateFormalUnknownDiscriminants;


   --
   -- Formal derived type cases:
   --

   generic
      type Formal (<>) is new FC51B00.Vector    -- Formal has an unknown disc.
        with private;                           -- part; ancestor is tagged.
   package TaggedAncestorUnknownDiscriminants is
      function Is_Definite return Boolean;
   end TaggedAncestorUnknownDiscriminants;


end CC51B03_0;


     --==================================================================--


package body CC51B03_0 is

   package body PrivateFormalUnknownDiscriminants is
      function Is_Definite return Boolean is
      begin
         if Formal'Definite then                -- Attribute used in "if"
            -- ...Execute algorithm #1...       -- condition inside subprogram.
            return True;
         else
            -- ...Execute algorithm #2...
            return False;
         end if;
      end Is_Definite;
   end PrivateFormalUnknownDiscriminants;


   package body TaggedAncestorUnknownDiscriminants is
      function Is_Definite return Boolean is
      begin
         return Formal'Definite;                -- Attribute used in return
      end Is_Definite;                          -- statement inside subprogram.
   end TaggedAncestorUnknownDiscriminants;


end CC51B03_0;


     --==================================================================--


with FC51B00;
package CC51B03_1 is

   subtype Spin_Type is Natural range 0 .. 3;

   type Extended_Vector (Spin : Spin_Type) is   -- Tagged type with
     new FC51B00.Vector with null record;       -- discriminant (indefinite).
      

end CC51B03_1;


     --==================================================================--


with FC51B00;   -- Indefinite subtype declarations.
with CC51B03_0; -- Generic package declarations.
with CC51B03_1;

with Report;
procedure CC51B03 is

   --
   -- Instances for formal private type with unknown discriminants:
   --

   package PrivateFormal_UndiscriminatedTaggedActual is new
     CC51B03_0.PrivateFormalUnknownDiscriminants (FC51B00.Vector);

   package PrivateFormal_ClassWideActual is new
     CC51B03_0.PrivateFormalUnknownDiscriminants (FC51B00.Vector'Class);

   package PrivateFormal_DiscriminatedTaggedActual is new
     CC51B03_0.PrivateFormalUnknownDiscriminants (FC51B00.Square_Pair);

   package PrivateFormal_DiscriminatedUndefaultedRecordActual is new
     CC51B03_0.PrivateFormalUnknownDiscriminants (FC51B00.Square);


   subtype Length is Natural range 0 .. 20;
   type Message (Len : Length := 0) is record   -- Record type with defaulted
      Text : String (1 .. Len);                 -- discriminant (definite).
   end record;
      
   package PrivateFormal_DiscriminatedDefaultedRecordActual is new
     CC51B03_0.PrivateFormalUnknownDiscriminants (Message);


   --
   -- Instances for formal derived tagged type with unknown discriminants:
   --

   package DerivedFormal_UndiscriminatedTaggedActual is new
     CC51B03_0.TaggedAncestorUnknownDiscriminants (FC51B00.Vector);

   package DerivedFormal_ClassWideActual is new
     CC51B03_0.TaggedAncestorUnknownDiscriminants (FC51B00.Vector'Class);

   package DerivedFormal_DiscriminatedTaggedActual is new
     CC51B03_0.TaggedAncestorUnknownDiscriminants (CC51B03_1.Extended_Vector);


begin
   Report.Test ("CC51B03", "Check that S'Definite returns true if the " &
                "actual corresponding to S is definite, and false otherwise");


   if not PrivateFormal_UndiscriminatedTaggedActual.Is_Definite then
      Report.Failed ("Formal private/unknown discriminants: wrong " &
                     "result for undiscriminated tagged actual");
   end if;  

   if PrivateFormal_ClassWideActual.Is_Definite then
      Report.Failed ("Formal private/unknown discriminants: wrong " &
                     "result for class-wide actual");
   end if;  

   if PrivateFormal_DiscriminatedTaggedActual.Is_Definite then
      Report.Failed ("Formal private/unknown discriminants: wrong " &
                     "result for discriminated tagged actual");
   end if;  

   if PrivateFormal_DiscriminatedUndefaultedRecordActual.Is_Definite then
      Report.Failed ("Formal private/unknown discriminants: wrong result " &
                     "for record actual with undefaulted discriminants");
   end if;  

   if not PrivateFormal_DiscriminatedDefaultedRecordActual.Is_Definite then
      Report.Failed ("Formal private/unknown discriminants: wrong result " &
                     "for record actual with defaulted discriminants");
   end if;  


   if not DerivedFormal_UndiscriminatedTaggedActual.Is_Definite then
      Report.Failed ("Formal derived/unknown discriminants: wrong result " &
                     "for undiscriminated tagged actual");
   end if;  

   if DerivedFormal_ClassWideActual.Is_Definite then
      Report.Failed ("Formal derived/unknown discriminants: wrong result " &
                     "for class-wide actual");
   end if;  

   if DerivedFormal_DiscriminatedTaggedActual.Is_Definite then
      Report.Failed ("Formal derived/unknown discriminants: wrong result " &
                     "for discriminated tagged actual");
   end if;  


   Report.Result;
end CC51B03;

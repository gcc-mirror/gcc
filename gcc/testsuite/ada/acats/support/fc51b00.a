-- FC51B00.A
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
--      This foundation declares a set of tagged and untagged indefinite
--      subtypes.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package FC51B00 is  -- Type definitions.

   subtype Size is Natural range 1 .. 4;

   type Matrix is array                                 -- Unconstrained array
     (Size range <>, Size range <>) of Integer;         -- type.

   type Square (Side : Size) is record                  -- Unconstrained record
      Mat : Matrix (1 .. Side, 1 .. Side);              -- with undefaulted
   end record;                                          -- discriminants.

   type Square_Pair (Dimension : Size) is tagged record -- Unconstrained tagged
      Left  : Square (Dimension);                       -- type.
      Right : Square (Dimension);
   end record;

   type Vector is tagged record                         -- Constrained tagged
      Mat : Matrix (1 .. 3, 1 .. 1);                    -- type (used to get
   end record;                                          -- class-wide type).

   generic  -- Template for a generic formal package.
      type Vectors (<>) is new Vector with private;     -- Type with unknown
   package Signature is end;                            -- discriminants.

end FC51B00;


-- No body for FC51B00;

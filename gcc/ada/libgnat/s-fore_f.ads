------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . F O R E _ F                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

--  This package contains the routine used for the Fore attribute of ordinary
--  fixed point types whose Small is the ratio of two Int values.

generic

   type Int is range <>;

   with procedure Scaled_Divide
          (X, Y, Z : Int;
           Q, R : out Int;
           Round : Boolean);

package System.Fore_F is
   pragma Pure;

   function Fore_Fixed
     (Lo, Hi, Num, Den : Int; Scale : Integer) return Natural;
   --  Compute Fore attribute value for an ordinary fixed point type. The
   --  parameters are the low and high bounds (in units of small), the small
   --  Num/Den and the associated scale, which is the smallest integer N such
   --  that 10**N * (Num/Den) is greater or equal to 1, if it is nonpositive.

end System.Fore_F;

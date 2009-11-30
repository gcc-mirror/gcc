------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                            G N A T . S H A 1                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2009, Free Software Foundation, Inc.             --
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

--  This package implaments the SHA-1 secure hash function as decsribed in
--  FIPS PUB 180-3. The complete text of FIPS PUB 180-3 can be found at:
--    http://csrc.nist.gov/publications/fips/fips180-3/fips180-3_final.pdf

--  See the declaration of GNAT.Secure_Hashes.H in g-sechas.ads for complete
--  documentation.

with GNAT.Secure_Hashes.SHA1;
with System;

package GNAT.SHA1 is new GNAT.Secure_Hashes.H
  (Block_Words    => GNAT.Secure_Hashes.SHA1.Block_Words,
   State_Words    => 5,
   Hash_Words     => 5,
   Hash_Bit_Order => System.High_Order_First,
   Hash_State     => GNAT.Secure_Hashes.SHA1.Hash_State,
   Initial_State  => GNAT.Secure_Hashes.SHA1.Initial_State,
   Transform      => GNAT.Secure_Hashes.SHA1.Transform);

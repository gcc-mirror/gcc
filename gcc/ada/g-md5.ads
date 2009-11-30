------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                             G N A T . M D 5                              --
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

--  This package implements the MD5 Message-Digest Algorithm as described in
--  RFC 1321. The complete text of RFC 1321 can be found at:
--          http://www.ietf.org/rfc/rfc1321.txt

--  See the declaration of System.Secure_Hashes.H in s-sechas.ads for complete
--  documentation.

with System.Secure_Hashes.MD5;

package GNAT.MD5 is new System.Secure_Hashes.H
  (Block_Words    => System.Secure_Hashes.MD5.Block_Words,
   State_Words    => 4,
   Hash_Words     => 4,
   Hash_Bit_Order => System.Low_Order_First,
   Hash_State     => System.Secure_Hashes.MD5.Hash_State,
   Initial_State  => System.Secure_Hashes.MD5.Initial_State,
   Transform      => System.Secure_Hashes.MD5.Transform);

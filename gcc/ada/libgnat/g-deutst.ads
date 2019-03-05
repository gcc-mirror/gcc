------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               G N A T . D E C O D E _ U T F 8 _ S T R I N G              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2007-2019, AdaCore                   --
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

--  This package provides a pre-instantiation of GNAT.Decode_String for the
--  common case of UTF-8 encoding. As noted in the documentation of that
--  package, this UTF-8 instantiation is efficient and specialized so that
--  it has only the code for the UTF-8 case. See g-decstr.ads for full
--  documentation on this package.

with GNAT.Decode_String;

with System.WCh_Con;

package GNAT.Decode_UTF8_String is
  new GNAT.Decode_String (System.WCh_Con.WCEM_UTF8);

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    G N A T . S O C K E T S . T H I N . T A S K _ S A F E _ N E T D B     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007, AdaCore                          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This package exports reentrant NetDB subprograms. This is the default
--  version, used on most platforms. The routines are implemented by importing
--  from C; see gsocket.h for details. Different versions are provided on
--  platforms where this functionality is implemented in Ada.

--  This package should not be directly with'ed by an application

package GNAT.Sockets.Thin.Task_Safe_NetDB is

   ----------------------------------------
   -- Reentrant network databases access --
   ----------------------------------------

   function Safe_Gethostbyname
     (Name     : C.char_array;
      Ret      : not null access Hostent;
      Buf      : System.Address;
      Buflen   : C.int;
      H_Errnop : not null access C.int) return C.int;

   function Safe_Gethostbyaddr
     (Addr      : System.Address;
      Addr_Len  : C.int;
      Addr_Type : C.int;
      Ret       : not null access Hostent;
      Buf       : System.Address;
      Buflen    : C.int;
      H_Errnop  : not null access C.int) return C.int;

   function Safe_Getservbyname
     (Name     : C.char_array;
      Proto    : C.char_array;
      Ret      : not null access Servent;
      Buf      : System.Address;
      Buflen   : C.int) return C.int;

   function Safe_Getservbyport
     (Port     : C.int;
      Proto    : C.char_array;
      Ret      : not null access Servent;
      Buf      : System.Address;
      Buflen   : C.int) return C.int;

private
   pragma Import (C, Safe_Gethostbyname, "__gnat_safe_gethostbyname");
   pragma Import (C, Safe_Gethostbyaddr, "__gnat_safe_gethostbyaddr");
   pragma Import (C, Safe_Getservbyname, "__gnat_safe_getservbyname");
   pragma Import (C, Safe_Getservbyport, "__gnat_safe_getservbyport");

end GNAT.Sockets.Thin.Task_Safe_NetDB;

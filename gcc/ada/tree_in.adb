------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              T R E E _ I N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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

with Atree;
with Csets;
with Elists;
with Fname;
with Lib;
with Namet;
with Nlists;
with Opt;
with Repinfo;
with Sinput;
with Stand;
with Stringt;
with Tree_IO;
with Uintp;
with Urealp;

procedure Tree_In (Desc : File_Descriptor) is
begin
   Tree_IO.Tree_Read_Initialize (Desc);
   Opt.Tree_Read;
   Atree.Tree_Read;
   Elists.Tree_Read;
   Fname.Tree_Read;
   Lib.Tree_Read;
   Namet.Tree_Read;
   Nlists.Tree_Read;
   Sinput.Tree_Read;
   Stand.Tree_Read;
   Stringt.Tree_Read;
   Uintp.Tree_Read;
   Urealp.Tree_Read;
   Repinfo.Tree_Read;
   Csets.Initialize;
end Tree_In;

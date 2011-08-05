------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              T R E E _ I N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

with Aspects;
with Atree;
with Csets;
with Elists;
with Fname;
with Lib;
with Namet;
with Nlists;
with Opt;
with Repinfo;
with Sem_Aux;
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
   Sem_Aux.Tree_Read;
   Sinput.Tree_Read;
   Stand.Tree_Read;
   Stringt.Tree_Read;
   Uintp.Tree_Read;
   Urealp.Tree_Read;
   Repinfo.Tree_Read;
   Aspects.Tree_Read;

   Csets.Initialize;
end Tree_In;

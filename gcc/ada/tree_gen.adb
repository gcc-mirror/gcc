------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             T R E E _ G E N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Aspects;
with Atree;
with Debug;
with Elists;
with Fname;
with Lib;
with Namet;
with Nlists;
with Opt;
with Osint.C;
with Repinfo;
with Sem_Aux;
with Sinput;
with Stand;
with Stringt;
with Uintp;
with Urealp;

with Tree_In;
pragma Warnings (Off, Tree_In);
--  We do not use Tree_In in the compiler, but it is small, and worth including
--  so that we get the proper license check for Tree_In when the compiler is
--  built. This will avoid adding bad dependencies to Tree_In and blowing ASIS.

procedure Tree_Gen is
begin
   if Opt.Tree_Output then
      Osint.C.Tree_Create;
      Opt.Tree_Write;

      --  For now, only write aspect specifications hash table if -gnatd.A set

      if Debug.Debug_Flag_Dot_AA then
         Aspects.Tree_Write;
      end if;

      Atree.Tree_Write;
      Elists.Tree_Write;
      Fname.Tree_Write;
      Lib.Tree_Write;
      Namet.Tree_Write;
      Nlists.Tree_Write;
      Sem_Aux.Tree_Write;
      Sinput.Tree_Write;
      Stand.Tree_Write;
      Stringt.Tree_Write;
      Uintp.Tree_Write;
      Urealp.Tree_Write;
      Repinfo.Tree_Write;
      Osint.C.Tree_Close;
   end if;
end Tree_Gen;

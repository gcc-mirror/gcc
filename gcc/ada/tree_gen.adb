------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             T R E E _ G E N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;
with Elists;
with Fname;
with Lib;
with Namet;
with Nlists;
with Opt;
with Osint;
with Repinfo;
with Sinput;
with Stand;
with Stringt;
with Uintp;
with Urealp;

procedure Tree_Gen is
begin
   if Opt.Tree_Output then
      Osint.Tree_Create;
      Opt.Tree_Write;
      Atree.Tree_Write;
      Elists.Tree_Write;
      Fname.Tree_Write;
      Lib.Tree_Write;
      Namet.Tree_Write;
      Nlists.Tree_Write;
      Sinput.Tree_Write;
      Stand.Tree_Write;
      Stringt.Tree_Write;
      Uintp.Tree_Write;
      Urealp.Tree_Write;
      Repinfo.Tree_Write;
      Osint.Tree_Close;
   end if;
end Tree_Gen;

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     G N A T . S O U R C E _ I N F O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--              Copyright (C) 2000 Ada Core Technologies, Inc.              --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides some useful utility subprograms that provide access
--  to source code information known at compile time. These subprograms are
--  intrinsic operations that provide information known to the compiler in
--  a form that can be embedded into the source program for identification
--  and logging purposes. For example, an exception handler can print out
--  the name of the source file in which the exception is handled.

package GNAT.Source_Info is
pragma Pure (Source_Info);

   function File return String;
   --  Return the name of the current file, not including the path information.
   --  The result is considered to be a static string constant.

   function Line return Positive;
   --  Return the current input line number. The result is considered
   --  to be a static expression.

   function Source_Location return String;
   --  Return a string literal of the form "name:line", where name is the
   --  current source file name without path information, and line is the
   --  current line number. In the event that instantiations are involved,
   --  additional suffixes of the same form are appended after the separating
   --  string " instantiated at ". The result is considered to be a static
   --  string constant.

   function Enclosing_Entity return String;
   --  Return the name of the current subprogram, package, task, entry or
   --  protected subprogram. The string is in exactly the form used for the
   --  declaration of the entity (casing and encoding conventions), and is
   --  considered to be a static string constant.
   --
   --  Note: if this function is used at the outer level of a generic
   --  package, the string returned will be the name of the instance,
   --  not the generic package itself. This is useful in identifying
   --  and logging information from within generic templates.

private
   pragma Import (Intrinsic, File);
   pragma Import (Intrinsic, Line);
   pragma Import (Intrinsic, Source_Location);
   pragma Import (Intrinsic, Enclosing_Entity);
end GNAT.Source_Info;

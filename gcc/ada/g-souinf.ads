------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     G N A T . S O U R C E _ I N F O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2013, Free Software Foundation, Inc.         --
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

--  This package provides some useful utility subprograms that provide access
--  to source code information known at compile time. These subprograms are
--  intrinsic operations that provide information known to the compiler in
--  a form that can be embedded into the source program for identification
--  and logging purposes. For example, an exception handler can print out
--  the name of the source file in which the exception is handled.

package GNAT.Source_Info is
   pragma Preelaborate;
   --  Note that this unit is Preelaborate, but not Pure, that's because the
   --  functions here such as Line are clearly not pure functions, and normally
   --  we mark intrinsic functions in a Pure unit as Pure, even though they are
   --  imported.
   --
   --  Historical note: this used to be Pure, but that was when we marked all
   --  intrinsics as not Pure, even in Pure units, so no problems arose.

   function File return String;
   --  Return the name of the current file, not including the path information.
   --  The result is considered to be a static string constant.

   function Line return Positive;
   --  Return the current input line number. The result is considered to be a
   --  static expression.

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
   --  considered to be a static string constant. The name is fully qualified
   --  using periods where possible (this is not always possible, notably in
   --  the case of entities appearing in unnamed block statements.)
   --
   --  Note: if this function is used at the outer level of a generic package,
   --  the string returned will be the name of the instance, not the generic
   --  package itself. This is useful in identifying and logging information
   --  from within generic templates.

private
   pragma Import (Intrinsic, File);
   pragma Import (Intrinsic, Line);
   pragma Import (Intrinsic, Source_Location);
   pragma Import (Intrinsic, Enclosing_Entity);
end GNAT.Source_Info;

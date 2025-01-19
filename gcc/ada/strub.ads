------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T R U B                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2021-2025, Free Software Foundation, Inc.       --
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

--  Package containing utility procedures related to Stack Scrubbing

with Types; use Types;

package Strub is
   type Strub_Mode is
     (Disabled,        --  Subprogram cannot be called from strub contexts
      At_Calls,        --  Subprogram strubbed by caller
      Internal,        --  Subprogram strubbed by wrapper
      Callable,        --  Subprogram safe to call despite no strub
      Unspecified,     --  Subprogram or data without strub annotation
      Enabled,         --  Data (variable or constant) that enables strub
      Not_Applicable); --  Entities that are not strub-capable
   --  This is the type that expresses decoded strub annotations

   --  We compare strub modes in the following circumstances:

   --  * subprogram definition vs specification
   --  * overriding vs overridden dispatch subprograms
   --  * implementation vs interface dispatch subprogram
   --  * renaming vs renamed subprogram
   --  * type resolution
   --  * explicit conversions

   --  Explicit conversions can convert between strub modes other than
   --  at-calls (see Compatible_Strub_Modes), but for the other cases
   --  above, we insist on identity of the strub modes (see
   --  Check_Same_Strub_Mode). Anything else would be
   --  troublesome.

   --  E.g., overriding a callable subprogram with a strub-disabled
   --  implementation would enable a subprogram that's unsafe to call
   --  in strub contexts to be called through a dispatching
   --  interface. An explicitly strub-disabled subprogram shall not be
   --  called from strub contexts, and a callable overriding
   --  subprogram would still seem not-callable, so accepting
   --  different modes would be surprising.

   --  We could relax the requirement for overriders from equality to
   --  compatibility, with the understanding that the dispatching ABI
   --  is what prevails. For renaming, however, if we don't require
   --  equality, it would have to encompass an implicit conversion.

   procedure Check_Same_Strub_Mode
     (Dest, Src : Entity_Id;
      Report    : Boolean := True);
   --  Check whether Dest and Src are subprograms or subprogram types
   --  annotated (or not) with the same strub mode. If Report is
   --  requested, and the strub modes are not equivalent, an error
   --  message is issued. Unspecified and Internal are considered
   --  equivalent, because Internal is an internal implementation
   --  detail. Unspecified decays to Disabled or Callable depending on
   --  -fstrub=(strict|relaxed), but this procedure does not take this
   --  decay into account, which avoids turning strub-equivalent
   --  declarations into incompatible ones at command-line changes.

   function Compatible_Strub_Modes
     (Dest, Src : Entity_Id) return Boolean;
   --  Return True if Dest and Src are subprograms or subprogram types
   --  annotated (or not) with ABI-compatible strub modes. At-calls is
   --  incompatible to other strub modes, because the back end
   --  internally modifies the signature of such subprograms, adding
   --  hidden parameters. Calling a subprogram through an
   --  access-to-subprogram object converted between strub-at-calls
   --  and other strub modes should be deemed equivalent to
   --  dereferencing an uninitialized access-to-data object, though
   --  one-way conversions might seem to work in some circumstances.
   --
   --  Unspecified, Disabled, Internal and Callable
   --  (access-to-)subprograms, on the other hand, can be safely but
   --  explicitly converted to each other, because these strub modes
   --  do not require signature changes; so it is possible to alter
   --  the caller-side stack scrubbing semantics of the call (e.g. to
   --  call a subprogram that isn't strub-callable from within a strub
   --  context, or to prevent it from being called through an access
   --  object) without any incompatibilities.

   procedure Copy_Strub_Mode (Dest, Src : Entity_Id);
   --  Copy the strub mode from Src to Dest, subprograms or subprogram
   --  types. Dest is required to not have a strub mode already set.

   function Explicit_Strub_Mode (Id : Entity_Id) return Strub_Mode;
   --  Return the strub mode associated with Id, that should refer to
   --  a subprogram, a data object, or a type.

   function Strub_Pragma_P (Item : Node_Id) return Boolean;
   --  Return True iff Item is a strub annotation, specifically, one
   --  introduced by pragma Machine_Attribute (Entity, "strub"[, "mode"]).

end Strub;

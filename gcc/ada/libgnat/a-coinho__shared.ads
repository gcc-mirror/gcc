------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . I N D E F I N I T E _ H O L D E R S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2013-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
------------------------------------------------------------------------------

--  This is an optimized version of Indefinite_Holders using copy-on-write.
--  It is used on platforms that support atomic built-ins.

private with Ada.Finalization;
private with Ada.Streams;

private with System.Atomic_Counters;
private with Ada.Strings.Text_Buffers;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Indefinite_Holders is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate (Indefinite_Holders);
   pragma Remote_Types (Indefinite_Holders);

   type Holder is tagged private
   with
      Preelaborable_Initialization;

   Empty_Holder : constant Holder;

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element
     (Container : in out Holder;
      New_Item  : Element_Type);

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type));

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with
      Implicit_Dereference => Element;

   type Reference_Type
     (Element : not null access Element_Type) is private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Holder) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Holder) return Reference_Type;
   pragma Inline (Reference);

   procedure Assign (Target : in out Holder; Source : Holder);

   function Copy (Source : Holder) return Holder;

   procedure Move (Target : in out Holder; Source : in out Holder);

   procedure Swap (Left, Right : in out Holder);

private

   use Ada.Finalization;
   use Ada.Streams;

   type Element_Access is access all Element_Type;

   type Holder_Access is access all Holder;

   type Shared_Holder is limited record
      Counter : System.Atomic_Counters.Atomic_Counter;
      Element : Element_Access;
   end record;

   type Shared_Holder_Access is access all Shared_Holder;

   procedure Reference (Item : not null Shared_Holder_Access);
   --  Increment reference counter

   procedure Unreference (Item : not null Shared_Holder_Access);
   --  Decrement reference counter, deallocate Item when counter goes to zero

   procedure Read
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Container : out Holder);

   procedure Write
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Container : Holder);

   type Holder is new Ada.Finalization.Controlled with record
      Reference : Shared_Holder_Access;
      Busy      : Natural := 0;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Holder);

   for Holder'Read use Read;
   for Holder'Write use Write;

   overriding procedure Adjust (Container : in out Holder);
   overriding procedure Finalize (Container : in out Holder);

   type Reference_Control_Type is new Controlled with record
      Container : Holder_Access;
   end record;

   overriding procedure Adjust (Control : in out Reference_Control_Type);
   pragma Inline (Adjust);

   overriding procedure Finalize (Control : in out Reference_Control_Type);
   pragma Inline (Finalize);

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is
      record
         Control : Reference_Control_Type :=
           raise Program_Error with "uninitialized reference";
         --  The RM says, "The default initialization of an object of
         --  type Constant_Reference_Type or Reference_Type propagates
         --  Program_Error."
      end record;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type);

   for Constant_Reference_Type'Write use Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type);

   for Constant_Reference_Type'Read use Read;

   type Reference_Type (Element : not null access Element_Type) is record
      Control : Reference_Control_Type :=
        raise Program_Error with "uninitialized reference";
      --  The RM says, "The default initialization of an object of
      --  type Constant_Reference_Type or Reference_Type propagates
      --  Program_Error."
   end record;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type);

   for Reference_Type'Write use Write;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type);

   for Reference_Type'Read use Read;

   Empty_Holder : constant Holder := (Controlled with null, 0);

end Ada.Containers.Indefinite_Holders;

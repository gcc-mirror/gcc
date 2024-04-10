------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       I N T E R F A C E S . C H E R I                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

--  This package provides bindings to CHERI intrinsics and some common
--  operations on capabilities.

with System;
with System.Storage_Elements;

package Interfaces.CHERI with
  Preelaborate,
  No_Elaboration_Code_All
is

   use type System.Storage_Elements.Integer_Address;

   subtype Capability is System.Address;

   type Bounds_Length is range 0 .. System.Memory_Size - 1 with
     Size => System.Word_Size;

   ----------------------------
   -- Capability Permissions --
   ----------------------------

   type Permissions_Mask is mod System.Memory_Size with
     Size => System.Word_Size;

   Global                  : constant Permissions_Mask := 16#0001#;
   Executive               : constant Permissions_Mask := 16#0002#;
   Mutable_Load            : constant Permissions_Mask := 16#0040#;
   Compartment_Id          : constant Permissions_Mask := 16#0080#;
   Branch_Sealed_Pair      : constant Permissions_Mask := 16#0100#;
   Access_System_Registers : constant Permissions_Mask := 16#0200#;
   Permit_Unseal           : constant Permissions_Mask := 16#0400#;
   Permit_Seal             : constant Permissions_Mask := 16#0800#;
   Permit_Store_Local      : constant Permissions_Mask := 16#1000#;
   Permit_Store_Capability : constant Permissions_Mask := 16#2000#;
   Permit_Load_Capability  : constant Permissions_Mask := 16#4000#;
   Permit_Execute          : constant Permissions_Mask := 16#8000#;
   Permit_Store            : constant Permissions_Mask := 16#1_0000#;
   Permit_Load             : constant Permissions_Mask := 16#2_0000#;

   function "and"
     (Cap  : Capability;
      Mask : Permissions_Mask)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_perms_and";
   --  Perform a bitwise-AND of a capability permissions and the specified
   --  mask, returning the new capability.

   function Get_Permissions (Cap : Capability) return Permissions_Mask with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_perms_get";
   --  Get the permissions of a capability

   function Clear_Permissions
     (Cap  : Capability;
      Mask : Permissions_Mask)
      return Capability is
        (Cap and not Mask);
   --  Clear the specified permissions of a capability, returning the new
   --  capability.

   function Has_All_Permissions
     (Cap         : Capability;
      Permissions : Permissions_Mask)
      return Boolean is
        ((Get_Permissions (Cap) and Permissions) = Permissions);
   --  Query whether all of the specified permission bits are set in a
   --  capability's permissions flags.

   -----------------------
   -- Common Intrinsics --
   -----------------------

   function Capability_With_Address
     (Cap  : Capability;
      Addr : System.Storage_Elements.Integer_Address)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_address_set";
   --  Return a new capability with the same bounds and permissions as Cap,
   --  with the address set to Addr.

   function Get_Address
     (Cap : Capability)
      return System.Storage_Elements.Integer_Address
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_address_get";
   --  Get the address of a capability

   procedure Set_Address
     (Cap  : in out Capability;
      Addr :        System.Storage_Elements.Integer_Address)
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_address_set";
   --  Set the address of a capability

   function Get_Base
     (Cap : Capability)
      return System.Storage_Elements.Integer_Address
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_base_get";
   --  Get the lower bound of a capability

   function Get_Offset (Cap : Capability) return Bounds_Length with
     Import,  Convention => Intrinsic,
     External_Name => "__builtin_cheri_offset_get";
   --  Get the difference between the address and the lower bound of a
   --  capability.

   procedure Set_Offset
     (Cap    : in out Capability;
      Offset :        Bounds_Length)
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_offset_set";
   --  Set the address relative to the lower bound of a capability

   function Capability_With_Offset
     (Cap   : Capability;
      Value : Bounds_Length)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_offset_set";
   --  Set the address relative to the lower bound of a capability, returning
   --  the new capability.

   function Increment_Offset
     (Cap   : Capability;
      Value : Bounds_Length)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_offset_increment";
   --  Increment the address of a capability by the specified amount,
   --  returning the new capability.

   function Get_Length (Cap : Capability) return Bounds_Length with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_length_get";
   --  Get the length of a capability's bounds

   function Clear_Tag (Cap : Capability) return Capability with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_tag_clear";
   --  Clear the capability validity tag, returning the new capability

   function Get_Tag (Cap : Capability) return Boolean with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_tag_get";
   --  Get the validty tag of a capability

   function Is_Valid (Cap : Capability) return Boolean is (Get_Tag (Cap));
   --  Check whether a capability is valid

   function Is_Invalid (Cap : Capability) return Boolean is
     (not Is_Valid (Cap));
   --  Check whether a capability is invalid

   function Is_Equal_Exact (A, B : Capability) return Boolean with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_equal_exact";
   --  Check for bit equality between two capabilities

   function Is_Subset (A, B : Capability) return Boolean with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_subset_test";
   --  Returns True if capability A is a subset or equal to capability B

   --------------------
   -- Bounds Setting --
   --------------------

   function Capability_With_Bounds
     (Cap    : Capability;
      Length : Bounds_Length)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_bounds_set";
   --  Narrow the bounds of a capability so that the lower bound is the
   --  current address and the upper bound is suitable for the Length,
   --  returning the new capability.
   --
   --  Note that the effective bounds of the returned capability may be wider
   --  than the range Get_Address (Cap) .. Get_Address (Cap) + Length - 1 due
   --  to capability compression, but they will always be a subset of the
   --  original bounds.

   function Capability_With_Exact_Bounds
     (Cap    : Capability;
      Length : Bounds_Length)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_bounds_set_exact";
   --  Narrow the bounds of a capability so that the lower bound is the
   --  current address and the upper bound is suitable for the Length,
   --  returning the new capability.
   --
   --  This is similar to Capability_With_Bounds but will clear the capability
   --  tag in the returned capability if the bounds cannot be set exactly, due
   --  to capability compression.

   function Capability_With_Address_And_Bounds
     (Cap     : Capability;
      Address : System.Storage_Elements.Integer_Address;
      Length  : Bounds_Length)
      return Capability is
        (Capability_With_Bounds
           (Capability_With_Address (Cap, Address), Length));
   --  Set the address and narrow the bounds of the capability so that the
   --  lower bound is Address and the upper bound is Address + Length,
   --  returning the new capability.
   --
   --  Note that the effective bounds of the returned capability may be wider
   --  than the range Address .. Address + Length - 1 due to capability
   --  compression, but they will always be a subset of the original bounds.

   function Capability_With_Address_And_Exact_Bounds
     (Cap     : Capability;
      Address : System.Storage_Elements.Integer_Address;
      Length  : Bounds_Length)
      return Capability is
        (Capability_With_Exact_Bounds
           (Capability_With_Address (Cap, Address), Length));
   --  Set the address and narrow the bounds of the capability so that the
   --  lower bound is Address and the upper bound is Address + Length,
   --  returning the new capability.
   --
   --  This is similar to Capability_With_Address_And_Bounds but will clear the
   --  capability tag in the returned capability if the bounds cannot be set
   --  exactly, due to capability compression.

   procedure Set_Bounds
     (Cap    : in out Capability;
      Length :        Bounds_Length)
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_bounds_set";
   --  Narrow the bounds of a capability so that the lower bound is the
   --  current address and the upper bound is suitable for the Length.
   --
   --  Note that the effective bounds of the output capability may be wider
   --  than the range Get_Address (Cap) .. Get_Address (Cap) + Length - 1 due
   --  to capability compression, but they will always be a subset of the
   --  original bounds.

   procedure Set_Exact_Bounds
     (Cap    : in out Capability;
      Length :        Bounds_Length)
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_bounds_set_exact";
   --  Narrow the bounds of a capability so that the lower bound is the
   --  current address and the upper bound is suitable for the Length.
   --
   --  This is similar to Set_Bounds but will clear the capability tag if the
   --  bounds cannot be set exactly, due to capability compression.

   procedure Set_Address_And_Bounds
     (Cap     : in out Capability;
      Address :        System.Storage_Elements.Integer_Address;
      Length  :        Bounds_Length)
   with
     Inline_Always;
   --  Set the address and narrow the bounds of the capability so that the
   --  lower bound is Address and the upper bound is Address + Length.
   --
   --  Note that the effective bounds of the output capability may be wider
   --  than the range Address .. Address + Length - 1 due to capability
   --  compression, but they will always be a subset of the original bounds.

   procedure Set_Address_And_Exact_Bounds
     (Cap     : in out Capability;
      Address :        System.Storage_Elements.Integer_Address;
      Length  :        Bounds_Length)
   with
     Inline_Always;
   --  Set the address and narrow the bounds of the capability so that the
   --  lower bound is Address and the upper bound is Address + Length.
   --
   --  This is similar to Set_Address_And_Bounds but will clear the capability
   --  tag if the bounds cannot be set exactly, due to capability compression.

   function Representable_Length (Length : Bounds_Length) return Bounds_Length
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_round_representable_length";
   --  Returns the length that a capability would have after using Set_Bounds
   --  to set the Length (assuming appropriate alignment of the base).

   function Representable_Alignment_Mask
     (Length : Bounds_Length)
      return System.Storage_Elements.Integer_Address
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_representable_alignment_mask";
   --  Returns a bitmask that can be used to align an address downwards such
   --  that it is sufficiently aligned to create a precisely bounded
   --  capability.

   function Align_Address_Down
     (Address : System.Storage_Elements.Integer_Address;
      Length  : Bounds_Length)
      return System.Storage_Elements.Integer_Address is
        (Address and Representable_Alignment_Mask (Length));
   --  Align an address such that it is sufficiently aligned to create a
   --  precisely bounded capability, rounding down if necessary.
   --
   --  Due to capability compression, the upper and lower bounds of a
   --  capability must be aligned based on the length of the bounds to ensure
   --  that the capability is representable. This function aligns an address
   --  down to the next boundary if it is not already aligned.

   function Capability_With_Address_Aligned_Down
     (Cap    : Capability;
      Length : Bounds_Length)
      return Capability is
        (Capability_With_Address
          (Cap, Align_Address_Down (Get_Address (Cap), Length)));
   --  Align a capability's address such that it is sufficiently aligned to
   --  create a precisely bounded capability, rounding down if necessary.
   --
   --  Due to capability compression, the upper and lower bounds of a
   --  capability must be aligned based on the length of the bounds to ensure
   --  that the capability is representable. This function aligns an address
   --  down to the next boundary if it is not already aligned.

   function Align_Address_Up
     (Address : System.Storage_Elements.Integer_Address;
      Length  : Bounds_Length)
      return System.Storage_Elements.Integer_Address
   with
     Inline;
   --  Align an address such that it is sufficiently aligned to create a
   --  precisely bounded capability, rounding upwards if necessary.
   --
   --  Due to capability compression, the upper and lower bounds of a
   --  capability must be aligned based on the length of the bounds to ensure
   --  that the capability is representable. This function aligns an address up
   --  to the next boundary if it is not already aligned.

   function Capability_With_Address_Aligned_Up
     (Cap    : Capability;
      Length : Bounds_Length)
      return Capability is
        (Capability_With_Address
           (Cap, Align_Address_Up (Get_Address (Cap), Length)));
   --  Align a capability's address such that it is sufficiently aligned to
   --  create a precisely bounded capability, rounding upwards if necessary.
   --
   --  Due to capability compression, the upper and lower bounds of a
   --  capability must be aligned based on the length of the bounds to ensure
   --  that the capability is representable. This function aligns an address up
   --  to the next boundary if it is not already aligned.

   ------------------------------------------
   -- Object Types, Sealing, and Unsealing --
   ------------------------------------------

   type Object_Type is
     range -2**(System.Word_Size - 1) .. +2**(System.Word_Size - 1) - 1 with
     Size => System.Word_Size;

   Type_Unsealed : constant Object_Type := 0;
   Type_Sentry   : constant Object_Type := 1;

   function Get_Object_Type (Cap : Capability) return Object_Type with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_type_get";
   --  Get the object type of a capability

   function Is_Sealed (Cap : Capability) return Boolean with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_sealed_get";
   --  Check whether a capability is sealed

   function Is_Unsealed (Cap : Capability) return Boolean is
     (not Is_Sealed (Cap));
   --  Check whether a capability is unsealed

   function Is_Sentry (Cap : Capability) return Boolean is
     (Get_Object_Type (Cap) = Type_Sentry);
   --  Check whether a capability is a sealed entry

   function Create_Sentry (Cap : Capability) return Capability with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_seal_entry";
   --  Create a sealed entry and return the new capability.

   function Seal
     (Cap         : Capability;
      Sealing_Cap : Capability)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_seal";
   --  Seal a capability with a sealing capability, by setting the object type
   --  of the capability to the Sealing_Cap's value, returning the sealed
   --  capability.

   function Unseal
     (Cap           : Capability;
      Unsealing_Cap : Capability)
      return Capability
   with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_unseal";
   --  Unseal a capability with an unsealing capability, by checking the object
   --  type of the capability against the Sealing_Cap's value, returning the
   --  unsealed capability.

   -----------------------------------
   -- Capability Register Accessors --
   -----------------------------------

   function Get_PCC return System.Address with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_program_counter_get";
   --  Get the Program Counter Capablity (PCC)

   function Get_DDC return System.Address with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_global_data_get";
   --  Get the Default Data Capability (DDC)

   function Get_CSP return System.Address with
     Import, Convention => Intrinsic,
     External_Name => "__builtin_cheri_stack_get";
   --  Get the Capability Stack Pointer (CSP)

   ---------------------------
   -- Capability Exceptions --
   ---------------------------

   Capability_Bound_Error : exception;
   --  An out-of-bounds access was attempted

   Capability_Permission_Error : exception;
   --  An attempted access exceeded the permissions granted by a capability

   Capability_Sealed_Error : exception;
   --  A sealed capability was dereferenced

   Capability_Tag_Error : exception;
   --  An invalid capability was dereferenced

end Interfaces.CHERI;

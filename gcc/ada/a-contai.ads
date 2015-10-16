------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       A D A . C O N T A I N E R S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

pragma Check_Name (Container_Checks);
pragma Check_Name (Tampering_Check);
--  The above checks are not in the Ada RM. They are added in order to allow
--  suppression of checks within containers packages. Suppressing
--  Tampering_Check suppresses the tampering checks and associated machinery,
--  which is very expensive. Suppressing Container_Checks suppresses
--  Tampering_Check as well as all the other (not-so-expensive) containers
--  checks.

private with Ada.Finalization;

package Ada.Containers is
   pragma Pure;

   type Hash_Type is mod 2**32;
   type Count_Type is range 0 .. 2**31 - 1;

   Capacity_Error : exception;

private

   type Tamper_Counts is record
      Busy : Natural := 0;
      Lock : Natural := 0;
   end record;

   --  Busy is positive when tampering with cursors is prohibited. Busy and
   --  Lock are both positive when tampering with elements is prohibited.

   type Tamper_Counts_Access is access all Tamper_Counts;
   for Tamper_Counts_Access'Storage_Size use 0;

   generic
   package Generic_Implementation is

      --  Generic package used in the implementation of containers.
      --  ???Currently used by Vectors; not yet by all other containers.

      --  This needs to be generic so that the 'Enabled attribute will return
      --  the value that is relevant at the point where a container generic is
      --  instantiated. For example:
      --
      --     pragma Suppress (Container_Checks);
      --     package My_Vectors is new Ada.Containers.Vectors (...);
      --
      --  should suppress all container-related checks within the instance
      --  My_Vectors.

      --  Shorthands for "checks enabled" and "tampering checks enabled". Note
      --  that suppressing either Container_Checks or Tampering_Check disables
      --  tampering checks. Note that this code needs to be in a generic
      --  package, because we want to take account of check suppressions at the
      --  instance. We use these flags, along with pragma Inline, to ensure
      --  that the compiler can optimize away the checks, as well as the
      --  tampering check machinery, when checks are suppressed.

      Checks : constant Boolean := Container_Checks'Enabled;
      T_Check : constant Boolean :=
        Container_Checks'Enabled and Tampering_Check'Enabled;

      --  Reference_Control_Type is used as a component of reference types, to
      --  prohibit tampering with elements so long as references exist.

      type Reference_Control_Type is
         new Finalization.Controlled with record
            T_Counts : Tamper_Counts_Access;
         end record
           with Disable_Controlled => not T_Check;

      overriding procedure Adjust (Control : in out Reference_Control_Type);
      pragma Inline (Adjust);

      overriding procedure Finalize (Control : in out Reference_Control_Type);
      pragma Inline (Finalize);

      procedure Zero_Counts (T_Counts : out Tamper_Counts);
      pragma Inline (Zero_Counts);
      --  Set Busy and Lock to zero

      procedure Busy (T_Counts : in out Tamper_Counts);
      pragma Inline (Busy);
      --  Prohibit tampering with cursors

      procedure Unbusy (T_Counts : in out Tamper_Counts);
      pragma Inline (Unbusy);
      --  Allow tampering with cursors

      procedure Lock (T_Counts : in out Tamper_Counts);
      pragma Inline (Lock);
      --  Prohibit tampering with elements

      procedure Unlock (T_Counts : in out Tamper_Counts);
      pragma Inline (Unlock);
      --  Allow tampering with elements

      procedure TC_Check (T_Counts : Tamper_Counts);
      pragma Inline (TC_Check);
      --  Tampering-with-cursors check

      procedure TE_Check (T_Counts : Tamper_Counts);
      pragma Inline (TE_Check);
      --  Tampering-with-elements check

      -----------------
      --  RAII Types --
      -----------------

      --  Initialize of With_Busy increments the Busy count, and Finalize
      --  decrements it. Thus, to prohibit tampering with elements within a
      --  given scope, declare an object of type With_Busy. The Busy count
      --  will be correctly decremented in case of exception or abort.

      --  With_Lock is the same as With_Busy, except it increments/decrements
      --  BOTH Busy and Lock, thus prohibiting tampering with cursors.

      type With_Busy (T_Counts : not null access Tamper_Counts) is
        new Finalization.Limited_Controlled with null record
          with Disable_Controlled => not T_Check;
      overriding procedure Initialize (Busy : in out With_Busy);
      overriding procedure Finalize (Busy : in out With_Busy);

      type With_Lock (T_Counts : not null access Tamper_Counts) is
        new Finalization.Limited_Controlled with null record
          with Disable_Controlled => not T_Check;
      overriding procedure Initialize (Lock : in out With_Lock);
      overriding procedure Finalize (Lock : in out With_Lock);

      --  Variables of type With_Busy and With_Lock are declared only for the
      --  effects of Initialize and Finalize, so they are not referenced;
      --  disable warnings about that. Note that all variables of these types
      --  have names starting with "Busy" or "Lock". These pragmas need to be
      --  present wherever these types are used.

      pragma Warnings (Off, "variable ""Busy*"" is not referenced");
      pragma Warnings (Off, "variable ""Lock*"" is not referenced");

   end Generic_Implementation;

end Ada.Containers;

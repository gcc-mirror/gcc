------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                SYSTEM.MULTIPROCESSORS.DISPATCHING_DOMAINS                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2011-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  Body used on unimplemented targets, where the operating system does not
--  support setting task affinities.

package body System.Multiprocessors.Dispatching_Domains is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Freeze_Dispatching_Domains;
   pragma Export
     (Ada, Freeze_Dispatching_Domains, "__gnat_freeze_dispatching_domains");
   --  Signal the time when no new dispatching domains can be created. It
   --  should be called before the environment task calls the main procedure
   --  (and after the elaboration code), so the binder-generated file needs to
   --  import and call this procedure.

   -----------------
   -- Assign_Task --
   -----------------

   procedure Assign_Task
     (Domain : in out Dispatching_Domain;
      CPU    : CPU_Range := Not_A_Specific_CPU;
      T      : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task)
   is
      pragma Unreferenced (Domain, CPU, T);
   begin
      raise Dispatching_Domain_Error with "dispatching domains not supported";
   end Assign_Task;

   ------------
   -- Create --
   ------------

   function Create (First : CPU; Last : CPU_Range) return Dispatching_Domain is
      pragma Unreferenced (First, Last);
   begin
      return raise Dispatching_Domain_Error with
        "dispatching domains not supported";
   end Create;

   function Create (Set : CPU_Set) return Dispatching_Domain is
      pragma Unreferenced (Set);
   begin
      return raise Dispatching_Domain_Error with
        "dispatching domains not supported";
   end Create;

   -----------------------------
   -- Delay_Until_And_Set_CPU --
   -----------------------------

   procedure Delay_Until_And_Set_CPU
     (Delay_Until_Time : Ada.Real_Time.Time;
      CPU              : CPU_Range)
   is
      pragma Unreferenced (Delay_Until_Time, CPU);
   begin
      raise Dispatching_Domain_Error with "dispatching domains not supported";
   end Delay_Until_And_Set_CPU;

   --------------------------------
   -- Freeze_Dispatching_Domains --
   --------------------------------

   procedure Freeze_Dispatching_Domains is
   begin
      null;
   end Freeze_Dispatching_Domains;

   -------------
   -- Get_CPU --
   -------------

   function Get_CPU
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return CPU_Range
   is
      pragma Unreferenced (T);
   begin
      return Not_A_Specific_CPU;
   end Get_CPU;

   -----------------
   -- Get_CPU_Set --
   -----------------

   function Get_CPU_Set (Domain : Dispatching_Domain) return CPU_Set is
      pragma Unreferenced (Domain);
   begin
      return raise Dispatching_Domain_Error
        with "dispatching domains not supported";
   end Get_CPU_Set;

   ----------------------------
   -- Get_Dispatching_Domain --
   ----------------------------

   function Get_Dispatching_Domain
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return Dispatching_Domain
   is
      pragma Unreferenced (T);
   begin
      return System_Dispatching_Domain;
   end Get_Dispatching_Domain;

   -------------------
   -- Get_First_CPU --
   -------------------

   function Get_First_CPU (Domain : Dispatching_Domain) return CPU is
      pragma Unreferenced (Domain);
   begin
      return CPU'First;
   end Get_First_CPU;

   ------------------
   -- Get_Last_CPU --
   ------------------

   function Get_Last_CPU (Domain : Dispatching_Domain) return CPU_Range is
      pragma Unreferenced (Domain);
   begin
      return Number_Of_CPUs;
   end Get_Last_CPU;

   -------------
   -- Set_CPU --
   -------------

   procedure Set_CPU
     (CPU : CPU_Range;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task)
   is
      pragma Unreferenced (CPU, T);
   begin
      raise Dispatching_Domain_Error with "dispatching domains not supported";
   end Set_CPU;

end System.Multiprocessors.Dispatching_Domains;

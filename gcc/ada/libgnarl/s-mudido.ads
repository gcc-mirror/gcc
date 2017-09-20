------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                SYSTEM.MULTIPROCESSORS.DISPATCHING_DOMAINS                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time;

with Ada.Task_Identification;

private with System.Tasking;

package System.Multiprocessors.Dispatching_Domains is
   --  pragma Preelaborate (Dispatching_Domains);
   --  ??? According to AI 167 this unit should be preelaborate, but it cannot
   --  be preelaborate because it depends on Ada.Real_Time which is not
   --  preelaborate.

   Dispatching_Domain_Error : exception;

   type Dispatching_Domain (<>) is limited private;

   System_Dispatching_Domain : constant Dispatching_Domain;

   function Create (First : CPU; Last : CPU_Range) return Dispatching_Domain;

   function Get_First_CPU (Domain : Dispatching_Domain) return CPU;

   function Get_Last_CPU (Domain : Dispatching_Domain) return CPU_Range;

   type CPU_Set is array (CPU range <>) of Boolean;

   function Create (Set : CPU_Set) return Dispatching_Domain;

   function Get_CPU_Set (Domain : Dispatching_Domain) return CPU_Set;

   function Get_Dispatching_Domain
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return Dispatching_Domain;

   procedure Assign_Task
     (Domain : in out Dispatching_Domain;
      CPU    : CPU_Range := Not_A_Specific_CPU;
      T      : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task);

   procedure Set_CPU
     (CPU : CPU_Range;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);

   function Get_CPU
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return CPU_Range;

   procedure Delay_Until_And_Set_CPU
     (Delay_Until_Time : Ada.Real_Time.Time;
      CPU              : CPU_Range);

private
   type Dispatching_Domain is new System.Tasking.Dispatching_Domain_Access;

   System_Dispatching_Domain : constant Dispatching_Domain :=
                                 Dispatching_Domain
                                   (System.Tasking.System_Domain);
end System.Multiprocessors.Dispatching_Domains;

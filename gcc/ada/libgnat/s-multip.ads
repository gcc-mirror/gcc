------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . M U L T I P R O C E S S O R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is the top level unit for multiprocessor task support as defined by
--  ARM D.16. It provides the base types to enumerate CPUs and the
--  functionality to get the number of CPUs on the current system.

package System.Multiprocessors is
   pragma Preelaborate (Multiprocessors);

   type CPU_Range is range 0 .. 2 ** 16 - 1;

   subtype CPU is CPU_Range range 1 .. CPU_Range'Last;

   Not_A_Specific_CPU : constant CPU_Range := 0;

   function Number_Of_CPUs return CPU;
   --  Number of available CPUs

end System.Multiprocessors;

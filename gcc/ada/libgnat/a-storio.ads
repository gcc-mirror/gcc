------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . S T O R A G E _ I O                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;
with System.Storage_Elements;

generic
   type Element_Type is private;

package Ada.Storage_IO is
   pragma Preelaborate;

   Buffer_Size : constant System.Storage_Elements.Storage_Count :=
                   System.Storage_Elements.Storage_Count
                     ((Element_Type'Size + System.Storage_Unit - 1) /
                                                      System.Storage_Unit);

   subtype Buffer_Type is
     System.Storage_Elements.Storage_Array (1 .. Buffer_Size);

   ---------------------------------
   -- Input and Output Operations --
   ---------------------------------

   procedure Read (Buffer : Buffer_Type; Item : out Element_Type);

   procedure Write (Buffer : out Buffer_Type; Item : Element_Type);

   ----------------
   -- Exceptions --
   ----------------

   Data_Error : exception renames IO_Exceptions.Data_Error;

end Ada.Storage_IO;

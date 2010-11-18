with Ada.Containers.Vectors;

package Atomic4 is

   type String is limited null record;
   type String_Access is access all String;
   pragma Atomic (String_Access);

   type Reference is record
      Text : String_Access;
   end record;

   package Reference_Vectors is
     new Ada.Containers.Vectors (Natural, Reference);

   type Reader is tagged limited record
      Current_Reference : Reference;
      Reference_Stack   : Reference_Vectors.Vector;
   end record;

   procedure Next (Self : in out Reader'Class);

end Atomic4;

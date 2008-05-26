--  { dg-do compile }
package body Pak is
   pragma Suppress (Discriminant_Check);
   --  Suppress discriminant check to prevent the assignment from using
   --  the predefined primitive _assign.
   
   procedure Initialize (X : in out T) is begin null; end Initialize;
   procedure Finalize (X : in out T) is begin null; end Finalize;
   
   procedure Assign (X : out T'Class) is
      Y : T;
   begin
      T (X) := Y;
   end Assign;
end Pak;

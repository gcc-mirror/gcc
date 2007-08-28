--  { dg-do compile }

with Ada.Finalization; use Ada;
package ai_116 is
   pragma Preelaborate;
   type Buffer_Type is limited interface;

   type Handle is new Finalization.Limited_Controlled and Buffer_Type with
     private;
   pragma Preelaborable_Initialization(Handle);

   type Ptr is access all String;
   Null_Handle : constant Handle;

private
   type Handle is new Finalization.Limited_Controlled and Buffer_Type with
      record
         Data     : Ptr  := null;
      end record;

   Null_Handle : constant Handle :=
     (Finalization.Limited_Controlled with Data => null);
end ai_116;

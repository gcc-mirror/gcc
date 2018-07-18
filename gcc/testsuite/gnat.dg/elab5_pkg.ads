package Elab5_Pkg is
   procedure ABE_Call;

   generic
   package ABE_Gen is
      procedure Force_Body;
   end ABE_Gen;

   task type ABE_Task;

   --------------------------------------------------
   -- Call to call, instantiation, task activation --
   --------------------------------------------------

   function Elaborator_1 return Boolean;
   function Elaborator_2 return Boolean;
   function Elaborator_3 return Boolean;

   procedure Suppressed_Call_1;

   pragma Warnings ("L");
   procedure Suppressed_Call_2;
   pragma Warnings ("l");

   procedure Suppressed_Call_3;
   pragma Warnings (Off, Suppressed_Call_3);

   -----------------------------------------------------------
   -- Instantiation to call, instantiation, task activation --
   -----------------------------------------------------------

   function Elaborator_4 return Boolean;

   generic
   package Suppressed_Generic is
      procedure Force_Body;
   end Suppressed_Generic;

   -------------------------------------------------------------
   -- Task activation to call, instantiation, task activation --
   -------------------------------------------------------------

   function Elaborator_5 return Boolean;
   function Elaborator_6 return Boolean;

   task type Suppressed_Task;
end Elab5_Pkg;

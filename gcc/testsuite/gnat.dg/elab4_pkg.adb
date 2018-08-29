with Ada.Text_IO; use Ada.Text_IO;

package body Elab4_Pkg is

   --------------------------------------------------
   -- Call to call, instantiation, task activation --
   --------------------------------------------------

   procedure Suppressed_Call_1 is
      package Inst is new ABE_Gen;
      T : ABE_Task;
   begin
      ABE_Call;
   end Suppressed_Call_1;

   function Elaborator_1 return Boolean is
   begin
      pragma Warnings ("L");
      Suppressed_Call_1;
      pragma Warnings ("l");
      return True;
   end Elaborator_1;

   Elab_1 : constant Boolean := Elaborator_1;

   procedure Suppressed_Call_2 is
      package Inst is new ABE_Gen;
      T : ABE_Task;
   begin
      ABE_Call;
   end Suppressed_Call_2;

   function Elaborator_2 return Boolean is
   begin
      Suppressed_Call_2;
      return True;
   end Elaborator_2;

   Elab_2 : constant Boolean := Elaborator_2;

   -----------------------------------------------------------
   -- Instantiation to call, instantiation, task activation --
   -----------------------------------------------------------

   package body Suppressed_Generic is
      procedure Force_Body is begin null; end Force_Body;
      package Inst is new ABE_Gen;
      T : ABE_Task;
   begin
      ABE_Call;
   end Suppressed_Generic;

   function Elaborator_3 return Boolean is
      pragma Warnings ("L");
      package Inst is new Suppressed_Generic;
      pragma Warnings ("l");
   begin
      return True;
   end Elaborator_3;

   Elab_3 : constant Boolean := Elaborator_3;

   -------------------------------------------------------------
   -- Task activation to call, instantiation, task activation --
   -------------------------------------------------------------

   task body Suppressed_Task is
      package Inst is new ABE_Gen;
      T : ABE_Task;
   begin
      ABE_Call;
   end Suppressed_Task;

   function Elaborator_4 return Boolean is
      pragma Warnings ("L");
      T : Suppressed_Task;
      pragma Warnings ("l");
   begin
      return True;
   end Elaborator_4;

   Elab_4 : constant Boolean := Elaborator_4;

   procedure ABE_Call is
   begin
      Put_Line ("ABE_Call");
   end ABE_Call;

   package body ABE_Gen is
      procedure Force_Body is begin null; end Force_Body;
   begin
      Put_Line ("ABE_Gen");
   end ABE_Gen;

   task body ABE_Task is
   begin
      Put_Line ("ABE_Task");
   end ABE_Task;
end Elab4_Pkg;

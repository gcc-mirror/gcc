package body Inline15_Gen is
   function Initialize (Val : Inline15_Types.Enum) return Inline15_Types.Rec;
   procedure Print (Val : Inline15_Types.Rec);

   procedure Call_Func is
      Result : constant Inline15_Types.Rec := Func (Inline15_Types.Two);
   begin
      null;
   end Call_Func;

   function Func (Val : Inline15_Types.Enum) return Inline15_Types.Rec is
   begin
      return Result : constant Inline15_Types.Rec := Initialize (Val) do
         Print (Result);
      end return;
   end Func;

   function Initialize (Val : Inline15_Types.Enum) return Inline15_Types.Rec is
      pragma Warnings (Off);
      Result : Inline15_Types.Rec (Val);
      pragma Warnings (On);
   begin
      return Result;
   end Initialize;

   procedure Print (Val : Inline15_Types.Rec) is begin null; end Print;
end Inline15_Gen;

--  { dg-do compile }

with Ada.Finalization; use Ada.Finalization;

procedure Controlled8
  (Int_Input : Integer;
   Str_Input : String)
is
   type Ctrl is new Controlled with null record;
   type Integer_Ptr is access all Integer;
   type String_Ptr  is access all String;

   function Func (Val : Integer) return Ctrl is
   begin return Result : Ctrl; end Func;

   function Func (Val : String) return Ctrl is
   begin return Result : Ctrl; end Func;

   type Rec_1 (Val : Integer) is record
      Comp : Ctrl := Func (Val);
   end record;

   type Rec_2 (Val : access Integer) is record
      Comp : Ctrl := Func (Val.all);
   end record;

   type Rec_3 (Val : Integer_Ptr) is record
      Comp : Ctrl := Func (Val.all);
   end record;

   type Rec_4 (Val : access String) is record
      Comp : Ctrl := Func (Val.all);
   end record;

   type Rec_5 (Val : String_Ptr) is record
      Comp : Ctrl := Func (Val.all);
   end record;

   Int_Heap  : constant Integer_Ptr := new Integer'(Int_Input);
   Int_Stack : aliased  Integer     := Int_Input;
   Str_Heap  : constant String_Ptr  := new String'(Str_Input);
   Str_Stack : aliased  String      := Str_Input;

   Obj_1  : constant Rec_1 := (Val => Int_Input, others => <>);

   Obj_2  : constant Rec_2 := (Val => Int_Heap, others => <>);
   Obj_3  : constant Rec_2 := (Val => Int_Stack'Access, others => <>);
   Obj_4  : constant Rec_2 := (Val => new Integer'(Int_Input), others => <>);

   Obj_5  : constant Rec_3 := (Val => Int_Heap, others => <>);
   Obj_6  : constant Rec_3 := (Val => Int_Stack'Access, others => <>);
   Obj_7  : constant Rec_3 := (Val => new Integer'(Int_Input), others => <>);

   Obj_8  : constant Rec_4 := (Val => Str_Heap, others => <>);
   Obj_9  : constant Rec_4 := (Val => Str_Stack'Access, others => <>);
   Obj_10 : constant Rec_4 := (Val => new String'(Str_Input), others => <>);

   Obj_11 : constant Rec_5 := (Val => Str_Heap, others => <>);
   Obj_12 : constant Rec_5 := (Val => Str_Stack'Access, others => <>);
   Obj_13 : constant Rec_5 := (Val => new String'(Str_Input), others => <>);
begin
   null;
end Controlled8;

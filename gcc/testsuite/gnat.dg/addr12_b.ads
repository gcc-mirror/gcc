package Addr12_B is

   type Entry_Type is record
      Auto_Init : Boolean;
   end record;

   type Entry_Range  is range 1 .. 20;
   type Entries_Type is array (Entry_Range) of Entry_Type;

   Null_Entry : constant Entry_Type := Entry_Type'(Auto_Init => False);

   type Shared_Context_Type is limited private;

   function Initial_State return Shared_Context_Type
   with Volatile_Function;

private

   type Shared_Context_Type is limited record
      Data : Entries_Type;
   end record
   with Volatile;

end Addr12_B;

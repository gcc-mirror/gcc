package body Opt57 is

   type Phase_Enum is (None_Phase, FE_Init_Phase, FE_Phase);

   type Message_State is (No_Messages, Some_Messages);

   type Module_List_Array is array (Phase_Enum, Message_State) of List;

   type Private_Module_Factory is limited record
      Module_Lists : Module_List_Array;
   end record;

   type Element_Array is array (Positive range <>) of Module_Factory_Ptr;

   type Hash_Table is array (Positive range <>) of aliased Module_Factory_Ptr;

   type Heap_Data_Rec (Table_Last : Positive) is limited record
      Number_Of_Elements : Positive;
      Table              : Hash_Table (1 .. Table_Last);
   end record;

   type Heap_Data_Ptr is access Heap_Data_Rec;

   type Table is limited record
      Data : Heap_Data_Ptr;
   end record;

   function All_Elements (M : Table) return Element_Array is
      Result : Element_Array (1 .. Natural (M.Data.Number_Of_Elements));
      Last   : Natural := 0;
   begin
      for H in M.Data.Table'Range loop
         Last := Last + 1;
         Result (Last) := M.Data.Table(H);
      end loop;
      return Result;
   end;

   The_Factories : Table;

   subtype Language_Array is Element_Array;
   type Language_Array_Ptr is access Language_Array;
   All_Languages : Language_Array_Ptr := null;

   procedure Init is
   begin
      if All_Languages = null then
         All_Languages := new Language_Array'(All_Elements (The_Factories));
      end if;
   end;

   function Is_Empty (L : List) return Boolean is
   begin
      return Link_Constant (L.Next) = L'Unchecked_Access;
   end;

   function First (L : List) return Linkable_Ptr is
   begin
      return Links_Type (L.Next.all).Container.all'Access;
   end;

   procedure Update is
      Check_New_Dependences : Boolean := False;
   begin
      loop
         for Lang_Index in All_Languages'Range loop
            for Has_Messages in Message_State loop
               declare
                  L : List renames
                    All_Languages (Lang_Index).Priv.Module_Lists
                      (FE_Init_Phase, Has_Messages);
               begin
                  while not Is_Empty (L) loop
                     declare
                        Module_In_Init_State : constant Module_Ptr :=
                          Module_Ptr (First (L));
                        Pin_Dependence : Pinned (Module_In_Init_State);
                     begin
                        Check_New_Dependences := True;
                     end;
                  end loop;
               end;
            end loop;
         end loop;
         exit when not Check_New_Dependences;
      end loop;
   end;

end Opt57;

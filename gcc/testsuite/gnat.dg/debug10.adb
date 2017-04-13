-- PR debug/80321

-- { dg-do compile }
-- { dg-options "-O2 -g" }

with Debug10_Pkg; use Debug10_Pkg;

procedure Debug10 (T : Entity_Id) is

   procedure Inner (E : Entity_Id);
   pragma Inline (Inner);

   procedure Inner (E : Entity_Id) is
   begin
      if E /= Empty
         and then not Nodes (E + 3).Flag16
      then
         Debug10 (E);
      end if;
   end Inner;

   function Ekind (E : Entity_Id) return Entity_Kind is
   begin
      return N_To_E (Nodes (E + 1).Nkind);
   end Ekind;

begin

   if T = Empty then
      return;
   end if;

   Nodes (T + 3).Flag16 := True;

   if Ekind (T) in Object_Kind then
      Inner (T);

   elsif Ekind (T) in Type_Kind then
      Inner (T);

      if Ekind (T) in Record_Kind then

         if Ekind (T) = E_Class_Wide_Subtype then
            Inner (T);
         end if;

      elsif Ekind (T) in Array_Kind then
         Inner (T);

      elsif Ekind (T) in Access_Kind then
         Inner (T);

      elsif Ekind (T) in Scalar_Kind then

         if My_Scalar_Range (T) /= Empty
           and then My_Test (My_Scalar_Range (T))
         then
            if My_Is_Entity_Name (T) then
               Inner (T);
            end if;

            if My_Is_Entity_Name (T) then
               Inner (T);
            end if;
         end if;
      end if;
   end if;
end;

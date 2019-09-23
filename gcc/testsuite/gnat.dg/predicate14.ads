generic
package Predicate14 with
  SPARK_Mode
is

   type Field_Type is (F_Initial, F_Payload, F_Final);

   type State_Type is (S_Valid, S_Invalid);

   type Cursor_Type (State : State_Type := S_Invalid) is private;

   type Cursors_Type is array (Field_Type) of Cursor_Type;

   type Context_Type is private;

   type Result_Type (Field : Field_Type := F_Initial) is
      record
         case Field is
            when F_Initial | F_Final =>
               null;
            when F_Payload =>
               Value : Integer;
         end case;
      end record;

   function Valid_Context (Context : Context_Type) return Boolean;

private

   function Valid_Type (Result : Result_Type) return Boolean is
     (Result.Field = F_Initial);

   type Cursor_Type (State : State_Type := S_Invalid) is
      record
         case State is
            when S_Valid =>
               Value : Result_Type;
            when S_Invalid =>
               null;
         end case;
      end record
      with Dynamic_Predicate =>
          (if State = S_Valid then Valid_Type (Value));

   type Context_Type is
      record
         Field : Field_Type := F_Initial;
         Cursors : Cursors_Type := (others => (State => S_Invalid));
      end record;

   function Valid_Context (Context : Context_Type) return Boolean is
     (for all F in Context.Cursors'Range =>
         (Context.Cursors (F).Value.Field = F));

   procedure Dummy;
end Predicate14;
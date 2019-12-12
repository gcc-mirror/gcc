package Limited3_Pkg is

   type Limited_Rec is limited
    null record;

   type Var_Rec (X : Integer) is record
      Name : String (1 .. X);
      Tag  : Limited_Rec;
   end record;

   type Rec (D : Boolean := True) is record
      case D is
         when True => L : Limited_Rec;
         when False => I : Integer;
      end case;
   end record;

   function F (I : Integer) return Rec;

   function FS (X : Integer) return Var_Rec;

   type Rec2 (D : Boolean := True) is record
      case D is
         when True => L : access Limited_Rec;
         when False => I : Integer;
      end case;
   end record;

   function F2 (I : Integer) return Rec2;
end Limited3_Pkg;

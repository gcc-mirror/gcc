with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Opt41_Pkg is

   type Enum is (One, Two, Three, Four, Five, Six);

   type Rec (D : Enum) is record
      case D is
         when One => 
            I : Integer;
         when Two | Five | Six =>
            S : Unbounded_String;
            case D is
               when Two => B : Boolean;
               when others => null;
            end case;
         when others =>
            null;
      end case;
   end record;

   type Rec_Ptr is access all Rec;

   function Rec_Write (R : Rec) return Unbounded_String;

   function Rec_Read (Str : String_Access) return Rec;

end Opt41_Pkg;

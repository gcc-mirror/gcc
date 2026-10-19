-- { dg-do compile }
-- { dg-skip-if "No Dwarf" { { hppa*-*-hpux* } && { ! lp64 } } }
-- { dg-options "-cargs -O0 -g -dA -margs" }

package Debug2 is

   type Root_T (Root_Disc : Boolean) is tagged record
      Name : String (1 .. 8);
      Flag_1 : Boolean;
      Flag_2 : Boolean;
   end record;

   type Child_T (Child_Disc : Boolean) is
     new Root_T (Root_Disc => Child_Disc) with record
      case Child_Disc is
         when True   => Child_Flag : Boolean;
         when others => null;
      end case;
   end record;

   type Grandchild_T (Grandchild_Disc : Boolean) is
     new Child_T (Child_Disc => Grandchild_Disc) with record
      case Grandchild_Disc is
         when True   => Grandchild_Flag : Boolean;
         when others => null;
      end case;
   end record;

   type Ptr is access Grandchild_T;

   A : Ptr;

end Debug2;

-- { dg-final {  scan-assembler-times "DW_AT_discr\n" 2 } }

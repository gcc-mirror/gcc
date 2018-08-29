-- { dg-do compile }
-- { dg-options "-gnatws" }

package Discr2 is

   package Dec is
      type T_DECIMAL (Prec : Integer := 1) is private;
   private
      type T_DECIMAL (Prec : Integer := 1) is record
         case Prec is
            when  1 .. 2 => Value : Integer;
            when others => null;
         end case;
      end record;
   end;

   type Value_T is record
      Bits  : Dec.T_DECIMAL(1);
   end record;
   for Value_T'size use 88;

   type Value_Entry_T is record
      Index : Integer;
      Value : Value_T;
   end record;

   type Value_Mode is (QI, HI, SI, DI, XI);
   for Value_Mode'size use 8;

   type Valid_Modes_T is array (Value_Mode) of Boolean;

   type Register_T is record
      Ventry : Value_Entry_T;
      Vmodes : Valid_Modes_T;
   end record;

   type Regid_T is (Latch, Acc);
   for Regid_T use (Latch => 0, Acc => 2);
   for Regid_T'Size use 8;

   type Regarray_T is array (Regid_T) of Register_T;

   type Machine_T (Up : Boolean := True) is record
      case Up is
         when True  => Regs : Regarray_T;
         when False => null;
       end case;
   end record;

end Discr2;
